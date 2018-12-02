# Once the package is created, use the following command
#
#          cmd/ctrl + shift + B
#
# to build this package


# To load module dev environment
# 
${{PACKAGE}}::dev_env_${{PACKAGE}}("${{MODULEID}}")



# After loading a module, preview it in RAVE
preview()

# To disable file monitor (Manually restart)
# uncomment and use the following line
# 
# preview(auto_reload = F)

