function initProgressMarkers() {
  $('.ProgressMarker').remove();
  $('.ProgressLine').remove();
  
  var $formPage = $('.FormPage');
  var $progressMarkers = $('.Answer').map(function() {
    var $progressMarker = $("<div class='ProgressMarker'/>");
    $progressMarker.addClass( $(this).hasClass('Answered') ? 'Answered' : 'Unanswered');
    $progressMarker.css('top', $(this).position().top);
    $formPage.append($progressMarker);
    return $progressMarker;
  });
  for (var i=1; i<$progressMarkers.length; i++) {
    var topMarkerY = $progressMarkers[i-1].position().top;
    var bottomMarkerY = $progressMarkers[i].position().top;
    var length = bottomMarkerY - topMarkerY;
    var isConnected = $progressMarkers[i-1].hasClass('Answered') &&
                      $progressMarkers[i].hasClass('Answered');
    
    var $progressLine = $("<div class='ProgressLine'>|</div>");
    $progressLine.addClass( isConnected ? 'Connected' : 'Disconnected');
    $progressLine.css('top', topMarkerY);    
    $progressLine.css('height', length);    
    $formPage.append($progressLine);
    console.log(isConnected, length);  
  }
}