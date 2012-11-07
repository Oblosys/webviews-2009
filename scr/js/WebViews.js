// make sure that jQuery is loaded before this file is loaded

function showDialog(dialogContent, buttons) {
  dlog(dialogContent);
  dlog(buttons);
  $dialog = $('<div class="dialog"></div>');
  $dialog.html(dialogContent);
  $buttonRow = $('<div class="dialogButtons"></div>');
  var commandNr = 0;
  for (var i=0; i<buttons.length; i++) {
    $buttonRow.append($('<input type=button value="'+buttons[i].name+'" onClick="dialogButtonClicked('+ i + ',' + buttons[i].command +')"></input>'));
  }
  $dialog.append($buttonRow);
  $dialogContainer = $('<div class="dialogContainer"></div>');
  $dialogContainer.append($dialog);
  $dialogBackground = $('<div class="dialogBackground">');
  $('body').append( $dialogBackground);
  $('body').append( $dialogContainer );
  $dialogBackground.fadeIn(100);
  $dialogContainer.fadeIn(100);
  $(document).on("keydown.dialog",function(e) {
    if (e.keyCode == 13)  
      dialogButtonClicked(0, buttons[0].command == "true" ? true : false);
    else if (e.keyCode == 27)  
      dialogButtonClicked(-1, false);
     // only handle escape key, no default for return key yet.
  });
}

function dialogButtonClicked(nr,command) { // command == false denotes a cancel button (for which nr is ignored)
  $(document).off("keydown.dialog");
  var $dialogContainer = $('.dialogContainer');
  var $dialogBackground = $('.dialogBackground');
  $dialogBackground.fadeOut(100, function() { // delay should be small, because background prevents editing
    $dialogBackground.remove();
  });
  $dialogContainer.fadeOut(100, function() {
    $dialogContainer.remove();
  });
  
  if (command) {
    queueCommand("DialogButtonPressed "+nr);
  }
}

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
  dlog(id, targetElt, $targetElt.height());
  var divWidth = $targetElt.width();
  var divHeight = $targetElt.height();
  var overlay = $('<div style="z-index: 100; position:absolute; width:'+divWidth+'px; height:'+divHeight+'px; background-color:grey; opacity:0.1; filter:alpha(opacity=10); /* For IE8 and earlier */"></div>');
  $targetElt.prepend(overlay);
  spinners.push({spinner: spinner, overlay: overlay});
}

function clearSpinners() {
	while (spinner = spinners.pop()) {
		dlog('Clearing spinner '+spinner);
		spinner.spinner.stop();
		spinner.overlay.remove();
	}
	
}

// Selects and deselect selectable views immediately, so we don't have to wait for the server response.
function selectSelectableView(viewId, viewIds) {
  for (var i = 0; i<viewIds.length; i++)
    if (viewIds[i]!=viewId)
      $('#'+viewIds[i]).removeClass('Selected').addClass('Deselected');
    
  $('#'+viewId).removeClass('Deselected').addClass('Selected');
}

(function($) { // from http://stackoverflow.com/questions/536814/insert-ellipsis-into-html-tag-if-content-too-wide
	$.fn.ellipsis = function() {
		return this.each(function() {
			var el = $(this);

			// after execution, the width and height are stored in ellipsisWidth/Height, so if these are equal
			// to the current width/height, we don't need to execute again.
			var isDirty = el.attr('ellipsisWidth') != el.width() || el.attr('ellipsisHeight') != el.height();
			
			if(el.css("overflow") == "hidden"  && isDirty) {
				dlog('Evaluating ellipsis on '+el.attr('id')+ ' for width: '+el.width()+' and height: '+el.height()
						 + ' ('+el.attr('ellipsisWidth')+','+el.attr('ellipsisHeight')+')');
				
				var text = el.html();
				var multiline = el.hasClass('multiline');
				var t = $(this.cloneNode(true))
								.hide()
								.css('position', 'absolute')
								.css('overflow', 'visible')
								.width(multiline ? el.width() : 'auto')
								.height(multiline ? 'auto' : el.height());

				el.after(t);

				function height() { return t.height() > el.height(); };
				function width() { return t.width() > el.width(); };

				var func = multiline ? height : width;

				var i = text.length / 2;
				var p = 0;

				while (i>0.5) {
					t.html(text.substr(0, p) + "...");
					if (func())
						p-=i;
					else
						p+=i;
					i = Math.floor(i / 2);
				}

				el.html( text.substr(0,p-1) + "..." ); // subtract one more, since binary search can end up one char too long (quick & dirty solution)
				el.attr('ellipsisWidth', el.width());
				el.attr('ellipsisHeight', el.height());
				t.remove();
			}
		});
	};
})(jQuery);
