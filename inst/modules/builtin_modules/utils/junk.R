utils = rave_tools()


utils$load_subject(subject_code = 'YAB', project_name = 'JUNK')

epoch = load_meta('epoch', 'JUNK', 'YAB', meta_name = 'YABa')
t = round(epoch$Time[epoch$Block == '008'] * utils$get_srate())



pre = 0; post = 2

sapply(t, function(tt){
  seq(tt - pre* utils$get_srate(), tt + post* utils$get_srate())
}) ->
    tp
s = utils$load_volt_data(block = '008', chl = 38)

s = s[as.vector(tp)]

pwelch(s, utils$get_srate(), log = 'xy', xlim = c(0,3), window = 16, noverlap = 8)


diagnose_signal(s, srate = utils$get_srate())
