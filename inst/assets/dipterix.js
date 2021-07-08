/*
** Javascript to handle events
** Author: Zhengjia Wang
** Date 6/2/2017
** Last Updated: 5/2/2019
*/

// Uses jQuery $.ready
$(document).ready(function(){

  const _this = this;

	// elements ".force-recalculate" might be generated in the middle, so listen on the whole document
	$(document).on("click", ".force-recalculate", () => {
	  // 1) Trigger the resize event (so images are responsive and resize)
	  $(window).trigger("resize");
	});

  // listeners to hide inputs
	$(document).on('click', '.nav-link[data-toggle="rave-toggle-inputs"]', () => {
	  $('.content-wrapper').toggleClass('rave-hide-inputs');
	});

  // Nav-link to toggle control panel
	$(document).on('click', "[data-toggle='control-sidebar']", () => {
	  $('body').toggleClass('rave-control-open');
	});
	
	$(document).on('click', '.rave-app-btn.rave-restart-btn', () => {
	  window.location.reload(true);
	});


  // Zoom in panels
	$(document).on("click", ".rave-elastic-btn", (evt) => {
	  // find its parent div and add class "rave-elastic"
	  const cls_name = "rave-elastic-active";
	  let el = $(evt.target),
	      target_selector, pa;
	      
	  if( evt.target.tagName === 'I' ){
	    el = el.parent();
	  }
	  target_selector = el.attr('data-target');

	  if(target_selector !== undefined){
	    pa = el.closest(target_selector);
	  }else{
      pa = el.parent();
	  }

	  pa.addClass('rave-elastic');

	  if(pa.hasClass(cls_name)){
	    pa.removeClass(cls_name);
	    // el.html('<i class="fa fa-expand" aria-hidden="true"></i>');
	  }else{
	    pa.addClass(cls_name);
	    // el.html('<i class="fa fa-compress" aria-hidden="true"></i>');
	  }
	});

	// Making form button with "primary" and data-value='13' listen to keyboard "enter"
	// This is tricky since forms might be generated dyynamically. We have to use 'e.target'
	// to identify where the "enter" key occurs.
	$(document).keydown((e) => {
	  var enter_hit = (e.which && e.which == 13) || (e.keyCode && e.keyCode == 13),
	      ctrl_hit = (e.metaKey || e.ctrlKey),
	      shift_hit = e.shiftKey,
	      alt_key = e.altKey,
	      key_code = e.which || e.keyCode;
	   // send to shiny
     Shiny.onInputChange('..keyboard_event..', {
       ctrl_hit: ctrl_hit,
       shift_hit: shift_hit,
       enter_hit: enter_hit,
       alt_hit : alt_key,
       key_code : key_code,
       timestamp: new Date() // String(new Date())
     });
	   return true;
  });


  const calculate_client_size = () => {
    if( !Shiny.onInputChange ){ return; }
    // calculate available width and height
    let avail_width = screen.availWidth || window.outerWidth;
    let avail_height = screen.availHeight || window.outerHeight;
    avail_width = avail_width - window.outerWidth + window.innerWidth;
    avail_height = avail_height - window.outerHeight + window.innerHeight;
    Shiny.onInputChange("..client_size..", {
      // maximized inner dimension
      available_size : [ avail_width, avail_height ],
      // current window dimension
      window_size    : [ window.innerWidth, window.innerHeight ],
      pixel_ratio    : window.devicePixelRatio
    });
  };
  $(document).on("shiny:connected", (e) => {
      calculate_client_size();
  });
  $(window).resize((e) => {
      calculate_client_size();
  });


});


const _this = {};

Shiny.addCustomMessageHandler("rave_set_id", (key) => {
  _this.current_key = key;
});

Shiny.addCustomMessageHandler('rave_set_theme', (params) => {
  let theme = params.theme || 'light';
  $('body').removeClass('rave-dark');
  $('body').removeClass('rave-light');
  $('body').addClass('rave-' + theme);
});

Shiny.addCustomMessageHandler("rave_enable_button", (data) => {
  const el = document.getElementById(data.element_id);
  if( el ){
    el.disabled = false;
  }
});

Shiny.addCustomMessageHandler("rave_disable_button", (data) => {
  const el = document.getElementById(data.element_id);
  if( el ){
    el.disabled = true;
  }
});

Shiny.addCustomMessageHandler("rave_set_storage", (data) => {
  const module_id = data.module_id,
      storage_key = data.storage_key,
      inputId = data.inputId,
      expr = data.expr,
      current_key = data.current_key;

  // Check data types
  if(typeof(storage_key) === 'string' && typeof(module_id) === 'string' && typeof(expr) === 'string'){

    const ls_key = 'rave-storage-inputs-' + storage_key;

    // Need to load previous sets
    let saved = localStorage.getItem(ls_key);
    saved = JSON.parse( saved ) || {};
    saved[ module_id ] = expr;
    saved.__changed_by = current_key;
    localStorage.setItem( ls_key, JSON.stringify( saved ) );
  }

});

// return as is whatever value is given (to update inputs)
Shiny.addCustomMessageHandler('rave_asis', (data) => {
  if( typeof(data.inputId) === 'string' ){
    Shiny.onInputChange(data.inputId, data.value);
  }
});

Shiny.addCustomMessageHandler('rave_close_tab', (data) => {
  if( typeof data.module_id !== 'string' ){ return; }
  const containerId = '#' + data.module_id + '_UI',
        headers = $(containerId + ' .box-header');
  headers.each((ii) => {
    const el_title = $(headers[ii]).find('.box-title');
    if( el_title.length > 0 ){
      let title = el_title[0].innerText;
      if(data.title === title){
        let btn = el_title
          .siblings('.box-tools')
          .find('.btn.btn-box-tool[data-widget="collapse"] .fa.fa-minus');
        if(btn.length > 0){
          $(btn[0]).click();
        }
      }
    }
  });
});

Shiny.addCustomMessageHandler('rave_open_tab', (data) => {
  if( typeof data.module_id !== 'string' ){ return; }
  const containerId = '#' + data.module_id + '_UI',
        headers = $(containerId + ' .box-header');
  headers.each((ii) => {
    const el_title = $(headers[ii]).find('.box-title');
    if( el_title.length > 0 ){
      let title = el_title[0].innerText;
      if(data.title === title){
        let btn = el_title
          .siblings('.box-tools')
          .find('.btn.btn-box-tool[data-widget="collapse"] .fa.fa-plus');
        if(btn.length > 0){
          $(btn[0]).click();
        }
      }
    }
  });
});



//Shiny.onInputChange(callback_id, re);
// Listen to localStorage for messages from other sessions
_this.last_cached = Date.now();
_this.bouncing = false;
_this.to_shiny_list = {};


_this.send_to_shiny = function(is_new = true){
  const now = Date.now();

  if( !_this.bouncing ){
    _this.bouncing = true;

    if(is_new){
      _this.last_cached = now;
    }

    setTimeout(() => {

      _this.bouncing = false;

      if(now - _this.last_cached < 600){
        _this.send_to_shiny(false);
      }else{
        for(let key in _this.to_shiny_list){
          Shiny.onInputChange(key, _this.to_shiny_list[key]);
        }
        _this.to_shiny_list = {};
      }

    }, 602);
  }else{
    _this.last_cached = now;
  }
};


window.addEventListener("storage", (evt) => {
  if( !evt || !evt.key ){
    return(null);
  }

  let msg = evt.newValue;
  const ls_key = 'rave-storage-inputs-' + _this.current_key;


  switch (evt.key) {
    case 'rave-messages':

      // key: "mytime", oldValue: "1552802489049", newValue: "1552802495278", url: "about:blank"
      msg = JSON.parse( msg );
      _this.to_shiny_list.__local_storage_message__= msg;
      _this.send_to_shiny();

      break;

    case ls_key:

      _this.to_shiny_list.__local_storage_inputs__= JSON.parse( msg );
      _this.send_to_shiny();

      localStorage.removeItem( ls_key );
      break;

    default:
      // code
  }
});

// Add message handlers
Shiny.addCustomMessageHandler("alertmessage", (v) => { alert(v) });
Shiny.addCustomMessageHandler("rave_sidebar_switch", (m) => {
  let el = $(".main-sidebar a[data-value='" + m.module_id + "']");
  if( el.length > 1 ){
    el = $(el[0]);
  }

  // I hate this but I couldn't find a better way to target the element
  // Find parent, must be an inactive treeview
  let pa = el.parents('.sidebar-menu>.treeview:not(.active)>.treeview-menu:not(.menu-open)');
  if( pa.length === 1 ){
    pa = pa.siblings('a');
    if( pa.length === 1 ){
      pa.click();
    }
  }

  el.click();

});










