NWBAttribute <- R6::R6Class(
  classname = 'NWBAttribute',
  public = list(
    name = '',
    dtype = 'text',
    value = NULL,
    initialize = function(conf){
      nms = c('name', 'dtype', 'value')
      for(nm in nms){
        self[[nm]] = conf[[nm]]
      }
    }
  ),
  active = list(
    mode = function(){
      switch (dtype,
        'text' = 'character',
        {
          'ANY'
        }
      )
    }
  )
)

NWBContainer <- R6::R6Class(
  classname = 'NWBContainer',
  public = list(
    namespace = NULL,
    name = 'unknown',
    doc = '',
    neurodata_type_def = '',
    neurodata_type_inc = '',
    is_definition = FALSE,
    groups = NULL,
    datasets = NULL,
    attributes = NULL,
    conf = NULL,
    print = function(..., indent = 0){
      indent_str = rep(' ', indent)
      cat(indent_str, self$name, ' - ', self$neurodata_type_def, ' - ',
          self$neurodata_type_inc, ' - ', self$namespace , '\n', sep = '')
      # if(length(self$groups)){
      #   cat(indent_str, 'Groups:\n', sep = '')
      #   lapply(self$groups, function(g){
      #     g$print(indent = indent + 2)
      #   })
      # }
      if(length(self$datasets)){
        lapply(self$datasets, function(g){
          if(g$name != 'unknown'){
            g$print(indent = indent + 2)
          }
        })
      }



    },
    initialize = function(conf, namespace){
      self$namespace = namespace
      all_nms = names(conf)
      nms = all_nms[all_nms %in% names(self)]
      lapply(nms, function(nm){
        self[[nm]] = conf[[nm]]
      })
      if('datasets' %in% all_nms){
        self$is_definition = TRUE
        datasets = lapply(conf$datasets, function(sub){
          NWBContainer$new(sub, namespace)
        })
        subnames = sapply(datasets, '[[', 'name')
        names(datasets) = subnames
        self$datasets = datasets
      }
      if('groups' %in% all_nms){
        self$is_definition = TRUE
        groups = lapply(conf$groups, function(sub){
          g = NWBContainer$new(sub, namespace)
          if(g$name != ''){
            self$datasets[[g$name]] = g
          }
          if((!length(g$neurodata_type_def) || g$neurodata_type_def == '')){
            return(NULL)
          }
          g
        })
        groups = groups[!vapply(groups, is.null, FALSE)]
        subnames = sapply(groups, '[[', 'neurodata_type_def')
        names(groups) = subnames
        self$groups = groups
      }
      if('attributes' %in% all_nms){
        attrs = lapply(conf$attributes, function(sub){
          NWBAttribute$new(sub)
        })
        subnames = sapply(attrs, '[[', 'name')
        names(attrs) = subnames
        self$attributes = attrs
      }

      names = c('name', 'full_name', 'default_name')
      names = names[names %in% all_nms]
      if(length(names)){
        self$name = conf[[names[[1]]]]
      }

      # Register definition
      if(length(conf$neurodata_type_def) && conf$neurodata_type_def != ''){
        .nwb_globals[[namespace]]$definitions[[conf$neurodata_type_def]] = self
      }

      # Register dataset
      if(length(conf$neurodata_type_def) && conf$neurodata_type_def != ''){
        .nwb_globals[[namespace]]$definitions[[conf$neurodata_type_def]] = self
      }
      Subject


      conf = conf[!all_nms %in% names(self)]
      self$conf = conf
    }
  )
)

.nwb_globals = new.env()

NWB_add_namespace <- function(yaml_path){
  root_dir = dirname(yaml_path)
  conf = as.list(raveio::load_yaml(yaml_path))


  lapply(conf$namespaces, function(nsconf){
    ns_name = nsconf$name
    schema = nsconf$schema

    this_env = environment()
    new_conf = list()

    lapply(schema, function(s){
      src = file.path(root_dir, s$source)
      if(length(src) && file.exists(src)){
        cont_sonf = as.list(raveio::load_yaml(src))
        this_env$new_conf$datasets = c(this_env$new_conf$datasets, cont_sonf$datasets)
        this_env$new_conf$groups = c(this_env$new_conf$groups, cont_sonf$groups)
      }
      NULL
    })

    .nwb_globals[[ns_name]] = new.env(parent = baseenv())
    .nwb_globals[[ns_name]]$definitions = list()

    new_conf$name = ns_name
    cont = NWBContainer$new(new_conf, ns_name)
    .nwb_globals[[ns_name]]$containers = cont
  })

  invisible()
}

NWB_namespace <- function(namespace){
  .nwb_globals[[namespace]]
}

NWB_dataset_def <- function(name, neurodata_type_def = NULL, namespace = 'core'){

  if(is.character(namespace)){
    if(length(neurodata_type_def) && !neurodata_type_def %in% names(.nwb_globals[[namespace]]$definitions)){
      return(NULL)
    }
    namespace = .nwb_globals[[namespace]]$containers
    if(length(namespace)){
      return(NWB_dataset_def(name, neurodata_type_def, namespace))
    }else{
      return(NULL)
    }
  }else{
    if(name %in% names(namespace$datasets)){
      return(namespace$datasets[[name]])
    }else{
      for(g in c(namespace$groups, namespace$datasets)){
        re = NWB_dataset_def(name, neurodata_type_def, namespace = g)
        if(!is.null(re)){
          return(re)
        }
      }

    }
  }

  return(NULL)
}



NWB_add_namespace(system.file('third_party/NWB/core/nwb.namespace.yaml', package='rave'))
NWB_add_namespace(system.file('third_party/NWB/ecog/ecog.namespace.yaml', package='rave'))


# ns = NWB_namespace('ecog')
# ns$definitions$ECoGSubject
# subject = NWB_dataset_def('subject', namespace = 'ecog')
#
# .nwb_globals$ecog$containers$datasets$subject
