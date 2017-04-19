$(document).ready(function() {
var monthNames = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ]; 
var dayNames= ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]

var newDate = testDate;
newDate.setDate(newDate.getDate());  
$('#Date').html(dayNames[newDate.getDay()] + ", " +  monthNames[newDate.getMonth()] + " " + newDate.getDate() + ", " + newDate.getFullYear());

setInterval( function() {
  newDate.setSeconds(newDate.getSeconds() + 1);
  var seconds = newDate.getSeconds();
  var minutes = newDate.getMinutes();
  var hours = newDate.getHours();

  $("#sec").html(( seconds < 10 ? "0" : "" ) + seconds);
  $("#min").html(( minutes < 10 ? "0" : "" ) + minutes);
  $("#period").html( hours < 12 ? "  AM" : "  PM" );

  hours = (hours < 12) ? hours : hours - 12;
  $("#hours").html(( hours < 10 ? "0" : "" ) + hours);
  },1000);
});
