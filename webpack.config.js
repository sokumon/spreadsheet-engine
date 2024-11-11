// /webpack.config.js
const path = require('path');

module.exports = {
  mode: "development",
  devtool: "inline-source-map",
  entry: {
    main: "./src/repl.ts",
  },
  target:"node",
  output: {
    path: path.resolve(__dirname, './build'),
    filename: "repl.js" // <--- Will be compiled to this single file
  },
  resolve: {
    extensions: [".ts", ".tsx", ".js"],
  },
  module: {
    rules: [
      { 
        test: /\.tsx?$/,
        exclude: [
          '/hacks/'
        ],
        loader: "ts-loader"
      }
    ]
  }
};