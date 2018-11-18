# Once the package is created, use the following command
#
#          cmd/ctrl + shift + B
#
# to build this package


# To load module ${{MODULELABEL}} (${{MODULEID}})
dev_env_${{PACKAGE}}("${{MODULEID}}")

# By default, the following call will load the first module
dev_env_${{PACKAGE}}()

# After loading a module, preview it in RAVE
preview()
