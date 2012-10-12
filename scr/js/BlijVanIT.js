function initProgressMarkers() {
  $('.ProgressMarker').remove();
  $('.ProgressLine').remove();
  
  var $formPage = $('.FormPage');
  var $progressMarkers = $('[Answer]').map(function() {
    var $progressMarker = $("<div class='ProgressMarker'/>");
    $progressMarker.attr('AnswerProgress', $(this).attr('Answer'));
    $progressMarker.css('top', $(this).position().top);
    $formPage.append($progressMarker);
    return $progressMarker;
  });
  
  if ($progressMarkers.length>0) {
    if ($formPage.hasClass('LastPage')) {
      var $sendButton = $('.SendButton button');

      // Safari and firefox use different methods of rendering buttons (Safari with padding and margin, Firefox with border)
      // this computation works for both methods and also looks okay in Chrome.
      
      var lastMarkerY = $progressMarkers[$progressMarkers.length-1].position().top;
      var isConnected = !$sendButton.is(':disabled');
      
      var $verticalLn = mkVerticalProgressLine(lastMarkerY, buttonMiddleY($sendButton)+2, isConnected);
      // can't be bothered to figure out why this +2 is necessary, but otherwise the line is too high
      $formPage.append($verticalLn);
      
      // progress line must connect exactly to button because disabled button is transparent.
      // We subtract 1 because Firefox shows a gap otherwise, and a 1 pixel overlap on Safari & Chrome is hardly visible.
      $formPage.append(mkHorizontalProgressLine( $sendButton.position().left + parseInt( $sendButton.css('margin-left')) + $sendButton.outerWidth()
                                                 - 1
                                               , $verticalLn.position().left
                                               , $verticalLn.position().top + $verticalLn.outerHeight()-2
                                               , isConnected) );
    } else {
      var $nextButton = $('.NextButton button');

                         
      var firstMarkerY = $progressMarkers[0].position().top;
      var isConnected = !$nextButton.is(':disabled');
    
      console.log($nextButton.outerWidth() + ' ' + (100+ parseInt( $nextButton.css('margin-top'))));
      var $verticalLn = mkVerticalProgressLine(buttonMiddleY($nextButton), firstMarkerY, isConnected);
      $formPage.append($verticalLn);
      $formPage.append(mkHorizontalProgressLine( $nextButton.position().left + parseInt( $nextButton.css('margin-left')) + $nextButton.outerWidth()
                                                 - 1
                                               , $verticalLn.position().left
                                               , $verticalLn.position().top
                                               , isConnected) );
    }
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
  console.log('\n\n\nposition is '+$progressLine.position().left);
}
function mkHorizontalProgressLine(leftX, rightX, y, isConnected) {
  var $progressLine = $("<div class='ProgressLine' orientation='Horizontal'></div>");
  $progressLine.addClass( isConnected ? 'Connected' : 'Disconnected');
  $progressLine.css('left', leftX);    
  $progressLine.css('width', rightX-leftX);    
  $progressLine.css('top', y);    
  return $progressLine;
}

function buttonMiddleY($button) {
  return $button.position().top +
         parseInt( $button.css('margin-top')) +
         parseInt( $button.css('border-top-width')) + 
         (parseInt( $button.css('padding-top')) + $button.height() + parseInt( $button.css('padding-bottom')))/2
         - 1; // -1 because of the width 2 of the line
}