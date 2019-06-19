var path = require("path");
var webpack = require("webpack");
var MonacoWebpackPlugin = require("monaco-editor-webpack-plugin");

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

var babelOptions = {
    presets: ["@babel/preset-react"],
    plugins: ['@babel/plugin-proposal-class-properties']
};

var isProduction = process.argv.indexOf("-w") < 0;
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

var basicConfig = {
  node: {
    __dirname: false,
    __filename: false
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: "fable-loader",
          options: {
            babel: babelOptions,
            define: isProduction ? ["DEBUG"] : ["DEBUG", "WATCH"]
          }
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: babelOptions
        },
      },
            {
            test: /\.css$/,
            use: ['style-loader', 'css-loader' ]
            }
    ]
  }
};

const CopyWebpackPlugin = require('copy-webpack-plugin');
//const UglifyJSPlugin = require('uglifyjs-webpack-plugin')

var mainConfig = Object.assign({
  target: "electron-main",
  entry: resolve("src/Main/Main.fsproj"),
  output: {
    path: resolve("."),
    filename: "main.js"
  },
}, basicConfig);

var rendererConfig = Object.assign({
  plugins: [

    //new UglifyJSPlugin(),
    new webpack.optimize.LimitChunkCountPlugin({ maxChunks: 1 }),
    new MonacoWebpackPlugin({
    //                                    languages:["javascript","css","html","json"],
        features:['accessibilityHelp', 'bracketMatching', 'caretOperations', 'clipboard', 'codeAction', 'codelens', 'colorDetector', 'comment', 'contextmenu', 'coreCommands', 'cursorUndo', 'dnd', 'find', 'folding', 'fontZoom', 'format', 'goToDefinitionCommands', 'goToDefinitionMouse', 'gotoError', 'gotoLine', 'hover', 'inPlaceReplace', 'inspectTokens', 'iPadShowKeyboard', 'linesOperations', 'multicursor', 'parameterHints', 'quickCommand', 'quickOutline', 'referenceSearch', 'rename', 'smartSelect', 'snippets', 'toggleHighContrast', 'toggleTabFocusMode', 'transpose', 'wordHighlighter', 'wordOperations', 'wordPartOperations']
//                                    ["accessibilityHelp", "bracketMatching", "caretOperations", "clipboard", "codeAction", "codelens", "colorDetector", "comment", "contextmenu", "coreCommands", "cursorUndo", "dnd", "find", "folding", "fontZoom", "format", "goToDefinitionCommands", "goToDefinitionMouse", "gotoError", "gotoLine", "hover", "inPlaceReplace", "inspectTokens", "iPadShowKeyboard", "linesOperations", "links", "multicursor", "parameterHints", "quickCommand", "quickOutline", "referenceSearch", "rename", "smartSelect", "snippets", "suggest", "toggleHighContrast", "toggleTabFocusMode", "transpose", "wordHighlighter", "wordOperations", "wordPartOperations"]
                                    })
  ],
  target: "electron-renderer",
  devtool: "source-map",
  entry: resolve("src/Renderer/Renderer.fsproj"),
  output: {
    path: resolve("app/js"),
    filename: "renderer.js"
  },
  externals: {
    "monaco": "var monaco",
    "editor": "var editor",
    "fable-repl": "var Fable",
  }
}, basicConfig);

module.exports = [mainConfig, rendererConfig]
