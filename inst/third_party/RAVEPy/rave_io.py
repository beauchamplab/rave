import pandas as pd
import numpy as np
import h5py       
def rave_csv_reader(file, header='infer', dtype=None, ordered=True, na_values='', **kwargs):
  df = pd.read_csv(file, sep=',', header=header, dtype = dtype, na_values = na_values, **kwargs)
  
  if ordered and isinstance(dtype, dict):
    columns = df.columns.tolist()
    firstc = list(dtype.keys())
    firstc = [x for x in firstc if x in columns]
    firstc.extend([x for x in columns if not x in firstc and x != 'Unnamed: 0'])
    df = df[firstc]
  
  return df
  

def create_virtual_data(file_pattern, x, entry_key, save_to):
  files = [file_pattern % el for el in x]
  files = [(el, f) for f, el in zip(files, x) if os.path.exists(f)]
  # entry_key = '/ref/power/008'
  # save_to = "/Users/beauchamplab/rave_data/data_dir/congruency/YAB/rave/data/power/virtual.h5"
  
  if len(files) == 0:
    print('No valid files found')
    return False
  
  # get file shape
  with h5py.File(files[0][1], 'r') as sample_f:
    sh = sample_f[entry_key].shape
    dtype = sample_f[entry_key].dtype
    
  layout = h5py.VirtualLayout(
    shape=(len(files),) + sh, 
    dtype=dtype
  )
  for i, file_dup in enumerate(files):
    filename = file_dup[1]
    el = file_dup[0]
    print(filename)
    vsource = h5py.VirtualSource(filename, entry_key, shape=sh)
    layout[i, :, :] = vsource
  
  with h5py.File(save_to, 'w', libver='latest') as f:
    f.create_virtual_dataset(entry_key, layout, fillvalue=np.nan)
    
  return True


