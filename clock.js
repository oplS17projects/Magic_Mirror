$(document).ready(function() {
var monthNames = [ "null", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ]; 
var monthNamesShort = [ "null", "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec" ];
var dayNames= ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]

var newDate = testDate;
newDate.setDate(newDate.getDate());  
$('#Date').html(monthNames[newDate.getMonth()] + " " + newDate.getDate() + ", " + newDate.getFullYear());

setInterval( function() {
  newDate.setSeconds(newDate.getSeconds() + 1);
  var seconds = newDate.getSeconds();
  var minutes = newDate.getMinutes();
  var hours = newDate.getHours();

  $("#sec").html(( seconds < 10 ? "0" : "" ) + seconds);
  $("#min").html(( minutes < 10 ? "0" : "" ) + minutes);
  $("#period").html( hours < 12 ? "  AM" : "  PM" );

  hours = (hours == 0) ? 12 : hours;
  hours = (hours < 13) ? hours : hours - 12;
  $("#hours").html(hours);
  },1000);
});
