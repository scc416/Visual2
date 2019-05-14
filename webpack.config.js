var path = require("path");
var webpack = require("webpack");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

var babelOptions = {
    presets: ["@babel/preset-react"],
//    presets: [
//        ["@babel/preset-env", {
//            "targets": {
//                "browsers": ["last 2 versions"]
//            },
//        "modules": false
//        }]
//    ],
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
            test:  /\.(sass|scss|css)$/,
            use: [
                  isProduction ? MiniCssExtractPlugin.loader : 'style-loader',
                  'css-loader',
                  'sass-loader',
                  ],
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
  }
}, basicConfig);

var rendererConfig = Object.assign({
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
