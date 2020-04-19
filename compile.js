const { compile } = require('node-elm-compiler');

compile(['./src/Main.elm'], {
  output: './dist/elm.js',
})
