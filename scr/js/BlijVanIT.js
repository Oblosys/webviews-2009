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
  for (var i=1; i<$progressMarkers.length; i++) {
    var topMarkerY = $progressMarkers[i-1].position().top;
    var bottomMarkerY = $progressMarkers[i].position().top;
    var length = bottomMarkerY - topMarkerY;
    var isConnected = $progressMarkers[i-1].attr('ProgressAnswer') == 'Answered' &&
                      $progressMarkers[i].attr('ProgressAnswer') == 'Answered';
    
    var $progressLine = $("<div class='ProgressLine'>|</div>");
    $progressLine.addClass( isConnected ? 'Connected' : 'Disconnected');
    $progressLine.css('top', topMarkerY);    
    $progressLine.css('height', length);    
    $formPage.append($progressLine);
    console.log(isConnected, length);  
  }
}