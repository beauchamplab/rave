# SUMA example

rafe_opts$set_options(
                          # These settings are set by default,
                          # only change those with different values

  data_dir = './data',
                          # Directory that stores data

  suma_path = '/Users/Zhengjia/abin',
                          # Path to find SUMA

  dyld_library_path = '/opt/X11/lib/flat_namespace',
                          # Library path to find suma command.

  suma_spec_file = 'test.spec',
                          # Command will be: suma -spec ./data/[SUBJECT_FOLDER]/suma/test.spec

  unbuffer_path = '/usr/local/bin',
                          # Path to find unbuffer command. If you don't have expect installed,
                          # `brew install expect` first

  suma_monitor_file = 'monitor.log',
                          # RAFE will monitor this file to get user clicks in SUMA

  suma_gifti_name_regex = 'electrode_[a-zA-Z0-9]+.gii'
                          # regex for electrode gifti filename
)


suma <- rsuma()
suma$launch_suma('subject_1')  # subject ID
                               # Now click/mark electrodes in SUMA, then

suma$update()                  # New electrodes marked since last time
suma$last_chosen               # Historical marked electrodes
