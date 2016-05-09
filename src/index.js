require( './styles/_reset.scss' );
require( './styles/cards.scss' );
require( './styles/style.scss' );


var Elm = require( './Main' );
Elm.embed(Elm.Main, document.getElementById('main'), { swap: false, startTime: new Date().getTime() });
