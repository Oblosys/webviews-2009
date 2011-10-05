// make sure that jQuery is loaded before this file is loaded

function addSpinner(id) {    
  var opts = {
    lines: 12, // The number of lines to draw
    length: 7, // The length of each line
    width: 4, // The line thickness
    radius: 10, // The radius of the inner circle
    color: '#000', // #rgb or #rrggbb
    speed: 1, // Rounds per second
    trail: 60, // Afterglow percentage
    shadow: false // Whether to render a shadow
  };
  
  var targetElt = document.getElementById(id);
  var spinner = new Spinner(opts).spin(targetElt);

  $targetElt = $('#'+id);
  console.log(id, targetElt, $targetElt.height());
  var divWidth = $targetElt.width();
  var divHeight = $targetElt.height();
  $targetElt.prepend('<div style="z-index: 100; position:absolute; width:'+divWidth+'px; height:'+divHeight+'px; background-color:grey; opacity:0.4; filter:alpha(opacity=40); /* For IE8 and earlier */"></div>');
}
