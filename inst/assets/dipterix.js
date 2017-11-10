/*
** Javascript to Set control sidebars
** Author: Zhengjia Wang
** Date 6/2/2017
*/


var AdminLTEOptions = {
  sidebarExpandOnHover: true,
  controlSidebarOptions: {
    //Which button should trigger the open/close event
    toggleBtnSelector: "[data-toggle='control-sidebar']",
    //The sidebar selector, use body
    selector: "body",
    //Enable slide over content
    slide: true
  }
};

$(document).ready(function(){
	// Whenever the sidebar expand/collapse button is clicked:
	$(document).on("click", ".force-recalculate", function() {
	  // 1) Trigger the resize event (so images are responsive and resize)
	  $(window).trigger("resize");
	});
});
