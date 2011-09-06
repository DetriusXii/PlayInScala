/* 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

$(document).ready(function() {
   $(".header ul li").hover(function() {
       $(this).addClass("selected");
   }, function () {
       $(this).removeClass("selected clicked");
   }).mousedown(function() {
      $(this).removeClass("selected");
      $(this).addClass("clicked");
   }).mouseup(function() {
      $(this).removeClass("clicked");
      $(this).addClass("selected");
   });
});

