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
      var sendButtonY = $sendButton.position().top + $sendButton.height()/2;
      var lastMarkerY = $progressMarkers[$progressMarkers.length-1].position().top;
      var isConnected = !$sendButton.is(':disabled');
    
      var $verticalLn = mkVerticalProgressLine(lastMarkerY, sendButtonY+2, isConnected);
      $formPage.append($verticalLn);
      $formPage.append(mkHorizontalProgressLine( $sendButton.position().left + $sendButton.width()/2
                                               , $verticalLn.position().left
                                               , $verticalLn.position().top + $verticalLn.height()-3
                                               , isConnected) );
    } else {
      var $nextButton = $('.NextButton button');
      var nextButtonY = $nextButton.position().top + $nextButton.height()/2;
      var firstMarkerY = $progressMarkers[0].position().top;
      var isConnected = !$nextButton.is(':disabled');
    
      var $verticalLn = mkVerticalProgressLine(nextButtonY, firstMarkerY, isConnected);
      $formPage.append($verticalLn);
      $formPage.append(mkHorizontalProgressLine( $nextButton.position().left + $nextButton.outerWidth()-1
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
