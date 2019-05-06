/*
* Author: Zhengjia Wang
* Date: 2/19/2018
* Last Updates: 6/14/2018
* This js file defines a customized shiny input component - compoundInput
* compoundInput takes in any shiny (native) inputs and can duplicate components
* This gives great flexibility to controling the number of unit components displayed.
*
* Originally designed for RAVE (R for Analysis and Visualizations of ECoG Data)
*
* Updates: updateCompountInput now only updates the number of components shown
*
* Requires: jQuery
*/


(function(){
var bindings = {
  'shiny::textInput': {
    'instant': 'change.textInputBinding',
    'debounce': 'keyup.textInputBinding input.textInputBinding',
    'binding_name': 'shiny.textInput',
    'scope': 'input[type="text"], input[type="search"], input[type="url"], input[type="email"]'
  },
  'shiny::textareaInput': {
    'instant': 'change.textInputBinding',
    'debounce': 'keyup.textInputBinding input.textInputBinding',
    'binding_name': 'shiny.textareaInput',
    'scope': 'textarea'
  },
  'shiny::passwordInput': {
    'instant': 'change.textInputBinding',
    'debounce': 'keyup.textInputBinding input.textInputBinding',
    'binding_name': 'shiny.passwordInput',
    'scope': 'input[type="password"]'
  },
  'shiny::numberInput': {
    'instant': 'change.textInputBinding',
    'debounce': 'keyup.textInputBinding input.textInputBinding',
    'binding_name': 'shiny.numberInput',
    'scope': 'input[type="number"]'
  },
  'shiny::checkboxInput': {
    'instant': 'change.checkboxInputBinding',
    'binding_name': 'shiny.checkboxInput',
    'scope': 'input[type="checkbox"]'
  },
  'shiny::sliderInput': {
    'instant': 'change.sliderInputBinding',
    'binding_name': 'shiny.sliderInput',
    'scope': 'input.js-range-slider'
  },
  'shiny::dateInput': {
    'instant': 'changeDate.dateInputBinding change.dateInputBinding',
    'debounce': 'keyup.dateInputBinding input.dateInputBinding',
    'binding_name': 'shiny.dateInput',
    'scope': '.shiny-date-input'
  },

  'shiny::dateRangeInput': {
    'instant': 'changeDate.dateRangeInputBinding change.dateRangeInputBinding',
    'debounce': 'keyup.dateRangeInputBinding input.dateRangeInputBinding',
    'binding_name': 'shiny.dateRangeInput',
    'scope': '.shiny-date-range-input'
  },

  'shiny::radioInput': {
    'instant': 'change.radioInputBinding',
    'binding_name': 'shiny.radioInput',
    'scope': '.shiny-input-radiogroup'
  },
  'shiny::selectInput': {
    'instant': 'change.selectInputBinding',
    'binding_name': 'shiny.selectInput',
    'scope': 'select'
  },
  'shiny::actionButton': {
    'instant': 'click.actionButtonInputBinding',
    'binding_name': 'shiny.actionButtonInput',
    'scope': '.action-button'
  },
  'shiny::checkboxGroupInput': {
    'instant': 'change.checkboxGroupInputBinding',
    'binding_name': 'shiny.checkboxGroupInput',
    'scope': '.shiny-input-checkboxgroup'
  },


  'shiny::bootstrapTabInput': {
    'instant': 'change shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding',
    'binding_name': 'shiny.bootstrapTabInput',
    'scope': 'ul.nav.shiny-tab-input'
  }
};

var getInnerId = function(inputId, subId, ind){
  return(inputId + '_' + subId + '_' + String(ind));
};

var addremove = function(){
  var val = $(this).attr('data-value'),
      target = $(this).attr('data-target'),
      targetui = $('#'+target),
      ncomp = parseInt(targetui.attr('data-value')),
      maxcomp = parseInt(targetui.attr('data-max'));

  console.log(targetui);
  if(val == '1' && ncomp < maxcomp){
    ncomp = ncomp + 1;
    var innerui = targetui.find('.rave-ui-compound-inner[data-value="'+ ncomp +'"]');
    innerui.removeClass('hidden');
    targetui.attr('data-value', ncomp);
    targetui.trigger('data-value-changed-instant');
  }else if(val == '0' && ncomp > 1){
    var innerui = targetui.find('.rave-ui-compound-inner[data-value="'+ ncomp +'"]');
    innerui.addClass('hidden');
    targetui.attr('data-value', ncomp - 1);
    targetui.trigger('data-value-changed-instant');
  }
};

$(document).on("click", ".rave-ui .rave-ui-compound-ctrl .btn", addremove);


var binding = new Shiny.InputBinding(),
    tmp = {};

binding.initialize = function(el){
  var elui = $(el),
      elid = elui.attr('id'),
      meta = JSON.parse(elui.find(".rave-ui-compound-meta").text()),
      maxcomp = parseInt(elui.attr('data-max')),
      fast_callback = function(e){
        tmp.triggered = Math.random();
        elui.trigger('data-value-changed-instant');
      },
      slow_callback = function(e){
        tmp.triggered = Math.random();
        elui.trigger('data-value-changed-slow');
      };
  for(var metaid in meta){
    var meta_func = meta[metaid],
        bds = bindings[meta_func];
    if(typeof(bds) !== 'undefined'){
      var debounded_evt = bds.debounce,
          instant_evt = bds.instant;

      for(ind = 1; ind <= maxcomp; ind++){
        innerid = '#' + getInnerId(elid, metaid, ind);
        if(typeof(debounded_evt) !== 'undefined'){
          $(innerid).on(debounded_evt, slow_callback);
        }

        if(typeof(instant_evt) !== 'undefined'){
          $(innerid).on(instant_evt, fast_callback);
        }
      }
    }
  }
};

binding.find = function(scope) {
  var el = $(scope).find(".rave-ui.rave-ui-compound");
  window.ww = el;
  return el;
};



binding.getValue = function(el) {
  /* Shiny.inputBindings.bindingNames["shiny.actionButtonInput"].binding.getValue
  Let's HACK!!! */

  var ui = $(el),
      re = [],
      ncomp = parseInt(ui.attr('data-value')),
      maxcomp = parseInt(ui.attr('data-max')),
      inputId = ui.attr('id'),
      meta = JSON.parse(ui.find(".rave-ui-compound-meta ").text());

  for(ind = 1; ind <= ncomp; ind++){
    var val = {};
    for(var subId in meta){
      var func_name = meta[subId],
          bd = bindings[func_name],
          ns_id = '#' + getInnerId(inputId, subId, ind);
      if(typeof(bd) !== 'undefined'){
        var bn = bd.binding_name,
            subel = bd.scope,
            shinybinds = Shiny.inputBindings.bindingNames[bn],
            newsel = subel.split(',').join(ns_id + ',') + ns_id,
            sub = ui.find(newsel);
        if(typeof(shinybinds) !== 'undefined' && sub.length > 0){
          // One problem when serializing selectInput: all elements will be part of list. Solution is to stringify values and deserialize in R
          // val[subId] = shinybinds.binding.getValue(sub[0]);
          val[subId] = JSON.stringify(shinybinds.binding.getValue(sub[0]));
        }
      }

    }
    re.push(val);
  }

  return {
    ncomp: parseInt(ui.attr('data-value')),
    meta: JSON.parse(ui.find(".rave-ui-compound-meta ").text()),
    timeStamp: tmp.triggered,
    maxcomp : parseInt(ui.attr('data-max')),
    inputId : ui.attr('id'),
    val : re
  };
};

var is_undefined = function(e){
  return(typeof(e) === 'undefined');
};

binding.receiveMessage = function(el, value) {

  var ui = $(el),
      ii = 0,
      re = [],
      ncomp = parseInt(ui.attr('data-value')),
      maxcomp = parseInt(ui.attr('data-max')),
      inputId = ui.attr('id'),
      meta = JSON.parse(ui.find(".rave-ui-compound-meta ").text()),
      which = parseInt(value.which);

  if(isNaN(which)){
    return(null);
  }

  for(ii = 0; ii < maxcomp; ii++){
    if(ii < which){
      ui.find('.rave-ui-compound-inner[data-value='+ (ii+1) +']').removeClass('hidden');
    }else{
      ui.find('.rave-ui-compound-inner[data-value='+ (ii+1) +']').addClass('hidden');
    }
  }
  ui.attr({'data-value': which});
  return(null);
};

binding.getState = function(el) {
  return {
    ncomp: parseInt($(el).attr('data-value'))
  };
};

binding.subscribe = function(el, callback) {
  $(el).on("data-value-changed-slow", function(e) {
    callback(true);
  });

  $(el).on("data-value-changed-instant", function(e) {
    callback(false);
  });
};

binding.unsubscribe = function(el) {
  $(el).off(".data-value-changed-instant");
  $(el).off(".data-value-changed-slow");
};

binding.getType = function() {
  return "rave.compoundInput";
};

binding.getRatePolicy = function() {
  return {
    policy: 'debounce',
    delay: 250
  };
};

Shiny.inputBindings.register(binding, "rave.compoundInput");
})();



