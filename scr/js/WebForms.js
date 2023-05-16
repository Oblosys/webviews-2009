var $previousAllAnswers = [];
var oldUpperButtonPos = {left: 0, top: 0};
var oldLowerButtonPos = {left: 0, top: 0};

function initProgressMarkers() {
  var $nextButtons = $('.NextButton button');
  var $upperNextButton = $($nextButtons[0]);
  var $lowerNextButton = $($nextButtons[1]);

  // compare with previous positions and answer progress, so we don't draw these when nothing changed
  var $allAnswers = $('[Answer]').map(function() {
	    return { y: getAnswerPosition($(this)), progress: $(this).attr('Answer')};
	  });

  var hasChanged = $allAnswers.length != $previousAllAnswers.length 
  				|| $upperNextButton.position().left != oldUpperButtonPos.left || $upperNextButton.position().top != oldUpperButtonPos.top
                || $lowerNextButton.position().left != oldLowerButtonPos.left || $lowerNextButton.position().top != oldLowerButtonPos.top; 

  if (!hasChanged)
    for (var i=0; i < $allAnswers.length && i < $previousAllAnswers.length; i++) {
	  hasChanged = hasChanged || $allAnswers[i].y != $previousAllAnswers[i].y
	   				          || $allAnswers[i].progress != $previousAllAnswers[i].progress;
    } 
  
  $previousAllAnswers = $allAnswers;
  oldUpperButtonPos = $upperNextButton.position();
  oldLowerButtonPos = $lowerNextButton.position();
  
  if(!hasChanged) return;
  
  $('.ProgressMarker').remove();
  $('.ProgressLine').remove();

  var $formPage = $('.FormPage');
  var $progressMarkers = $('[Answer]').map(function() {
    var $progressMarker = $("<div class='ProgressMarker'/>");
    $progressMarker.attr('AnswerProgress', $(this).attr('Answer'));
    $progressMarker.css('top', getAnswerPosition($(this)));
    $formPage.append($progressMarker);
    return $progressMarker;
  });
  
  if ($progressMarkers.length>0) {
	// Safari and firefox use different methods of rendering buttons (Safari with padding and margin, Firefox with border)
	// this computation works for both methods and also looks okay in Chrome.
	
    var firstMarkerY = $progressMarkers[0].position().top;
    var isConnected = !$upperNextButton.is(':disabled');
    
    //dlog($upperNextButton.outerWidth() + ' ' + (100+ parseInt( $upperNextButton.css('margin-top'))));
    var $verticalLn = mkVerticalProgressLine(buttonMiddleY($upperNextButton), firstMarkerY, isConnected);
    $formPage.append($verticalLn);
    $formPage.append(mkHorizontalProgressLine( $upperNextButton.position().left + parseInt( $upperNextButton.css('margin-left')) + $upperNextButton.outerWidth()
                                               - 1
                                             , $verticalLn.position().left
                                             , $verticalLn.position().top
                                             , isConnected) );
    
    var lastMarkerY = $progressMarkers[$progressMarkers.length-1].position().top;
    isConnected = !$lowerNextButton.is(':disabled');
    
    var $verticalLn = mkVerticalProgressLine(lastMarkerY, buttonMiddleY($lowerNextButton)+2, isConnected);
    // can't be bothered to figure out why this +2 is necessary, but otherwise the line is too high
    $formPage.append($verticalLn);
    
    // progress line must connect exactly to button because disabled button is transparent.
    // We subtract 1 because Firefox shows a gap otherwise, and a 1 pixel overlap on Safari & Chrome is hardly visible.
    $formPage.append(mkHorizontalProgressLine( $lowerNextButton.position().left + parseInt( $lowerNextButton.css('margin-left')) + $lowerNextButton.outerWidth()
                                               - 1
                                             , $verticalLn.position().left
                                             , $verticalLn.position().top + $verticalLn.outerHeight()-2
                                             , isConnected) );
  }
  
  for (var i=1; i<$progressMarkers.length; i++) {
    var topMarkerY = $progressMarkers[i-1].position().top;
    var bottomMarkerY = $progressMarkers[i].position().top;
    var isConnected = $progressMarkers[i-1].attr('AnswerProgress') == 'Answered' &&
                      $progressMarkers[i].attr('AnswerProgress') == 'Answered';
    
    $formPage.append(mkVerticalProgressLine(topMarkerY, bottomMarkerY, isConnected));
  }
}

function mkVerticalProgressLine(topY, bottomY, isConnected) {
  var $progressLine = $("<div class='ProgressLine' orientation='Vertical'></div>");
  $progressLine.addClass( isConnected ? 'Connected' : 'Disconnected');
  $progressLine.css('top', topY);    
  $progressLine.css('height', bottomY-topY);    
  return $progressLine;
  // dlog('\n\n\nposition is '+$progressLine.position().left);
}
function mkHorizontalProgressLine(leftX, rightX, y, isConnected) {
  var $progressLine = $("<div class='ProgressLine' orientation='Horizontal'></div>");
  $progressLine.addClass( isConnected ? 'Connected' : 'Disconnected');
  $progressLine.css('left', leftX);    
  $progressLine.css('width', rightX-leftX);    
  $progressLine.css('top', y);    
  return $progressLine;
}

function getAnswerPosition($answer) {
  if ($answer.attr('AnswerType')!='ButtonAnswer')
    return $answer.position().top;
  else { // For a ButtonAnswer, we take the position of the text in the first button
    $button = $answer.find('div');
    return $button.position().top + $button.outerHeight() / 2 - 8; // 8 is half of marker height (14/2) + border (1)
  }
}
function buttonMiddleY($button) {
  return $button.position().top +
         parseInt( $button.css('margin-top')) +
         parseInt( $button.css('border-top-width')) + 
         (parseInt( $button.css('padding-top')) + $button.height() + parseInt( $button.css('padding-bottom')))/2
         - 1; // -1 because of the width 2 of the line
}