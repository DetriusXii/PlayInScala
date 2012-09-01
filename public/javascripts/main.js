/* 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

$(document).ready(function() {
   $("header ul li").hover(function() {
       $(this).addClass("selected");
   }, function () {
       $(this).removeClass("selected clicked");
   }).mousedown(function() {
      $(this).removeClass("selected");
      $(this).addClass("clicked");
   }).mouseup(function() {
      $(this).removeClass("clicked");
      $(this).addClass("selected");
   }).click(function() {
      var url = $(this).attr("href");
      window.location = url;
   });
   
   $(".gamePanel").hover(function() {
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
   

   var footerFunction = function() {
   		   var footerHeight = $("footer").height();
		   var headerHeight = $(".header").height();
		   var contentHeight = $(".wrap").height();
		   var totalHeight = headerHeight + contentHeight;
		   var windowHeight = $(window).height();
		   
		   
		   if (contentHeight - footerHeight < windowHeight) {
		   		$("footer").offset({top: windowHeight - footerHeight, left: 0})
		   }
   }
   
   
   $(window).load(footerFunction).resize(footerFunction);
   
});

