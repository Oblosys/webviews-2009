// make sure that jQuery is loaded before this file is loaded
var spinners = new Array();

function addSpinner(id) {    
  var opts = {
    lines: 12, // The number of lines to draw
    length: 7, // The length of each line
    width: 3, // The line thickness
    radius: 7, // The radius of the inner circle
    color: '#000', // #rgb or #rrggbb
    speed: 0.5, // Rounds per second
    trail: 60, // Afterglow percentage
    shadow: false // Whether to render a shadow
  };
  
  var targetElt = document.getElementById(id);
  var spinner = new Spinner(opts).spin(targetElt);

  $targetElt = $('#'+id);
  console.log(id, targetElt, $targetElt.height());
  var divWidth = $targetElt.width();
  var divHeight = $targetElt.height();
  var overlay = $('<div style="z-index: 100; position:absolute; width:'+divWidth+'px; height:'+divHeight+'px; background-color:grey; opacity:0.1; filter:alpha(opacity=10); /* For IE8 and earlier */"></div>');
  $targetElt.prepend(overlay);
  console.log('Spinner '+spinner);
  spinners.push({spinner: spinner, overlay: overlay});
}

function clearSpinners() {
	for (i=0; i<spinners.length; i++) {
		console.log('Spinner '+spinners[i]);
		spinners[i].spinner.stop();
		spinners[i].overlay.remove();
	}
}