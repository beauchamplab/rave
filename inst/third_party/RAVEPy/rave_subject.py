import os
import re
import pandas as pd
import numpy as np
import h5py

def rave_csv_reader(file, header='infer', dtype=None, ordered=True, **kwargs):
  df = pd.read_csv(file, sep=',', header=header, dtype = dtype, **kwargs)
  
  if ordered and isinstance(dtype, dict):
    columns = df.columns.tolist()
    firstc = list(dtype.keys())
    firstc = [x for x in firstc if x in columns]
    firstc.extend([x for x in columns if not x in firstc and x != 'Unnamed: 0'])
    df = df[firstc]
  
  return df
  


class RAVEPath():
  
  @property
  def is_valid(self):
    return os.path.exists(self.path)
  
  def set_name(self, name):
    '''
    Remove path seps such as / or \\ and set self._name
    '''
    name = re.sub(r'/|\\', '', name)
    self._name = name
  
  def __init__(self, path, name, parent = None):
    self.set_name(name)
    self.path = path
    self._subs = {}
    if parent is None:
      self._depth = 0
    else:
      self._depth = parent._depth + 1
    
  def add(self, path, name, must_work = True):
    abs_path = os.path.join(self.path, path)
    if must_work and not os.path.exists(abs_path):
      raise Exception('Cannot find path [%s] relative to %s' % (path, self.path))
    
    if name is None:
      name = path
    self._subs[name] = self.__class__(abs_path, name, parent = self)
    return self._subs[name]
    
  def get_path(self, path):
    if not isinstance(path, list):
      path = re.split(r'/|\\', path)
    
    path = [x for x in path if x != '']
    
    if len(path) == 0:
      return self
    
    if path[0] in self._subs.keys():
      return self._subs[path[0]].get_path(path[1:])
    else:
      return None
  
  def keys(self):
    return self._subs.keys()
    
  def __str__(self):
    pad = '  ' * self._depth
    s = '%s-%s (%s)\n' % (pad, self._name, self.path)
    
    for k, v in self._subs.items():
      s = s + v.__str__()
    return s


class RAVESubject():
  
  def __init__(self, project_name, subject_code, root_dir):
    self._root_dir = root_dir
    self.project_name = project_name
    self.subject_code = subject_code
    
  def directories(self):
    rave_dir = RAVEPath(
      path = os.path.join(self._root_dir, self.project_name, self.subject_code, 'rave'), 
      name = 'rave_dir')
    preprocess_dir = rave_dir.add('preprocess', 'preprocess', must_work = False)
    preprocess_dir.add('voltage', 'voltage', must_work = False)
    
    data_dir = rave_dir.add('data', 'data', must_work = False)
    data_dir.add('power', 'power', must_work = False)
    data_dir.add('phase', 'phase', must_work = False)
    data_dir.add('voltage', 'voltage', must_work = False)
    data_dir.add('reference', 'reference', must_work = False)
    data_dir.add('cache', 'cache', must_work = False) \
            .add('cached_reference.csv', 'cached_reference', must_work = False)
    
    meta_dir = rave_dir.add('meta', 'meta', must_work = False)
    meta_dir.add('electrodes.csv', 'electrodes', must_work = False)
    meta_dir.add('frequencies.csv', 'frequencies', must_work = False)
    meta_dir.add('time_points.csv', 'time_points', must_work = False)
    
    # Add epochs and references
    if os.path.exists(meta_dir.path):
      meta_files = os.listdir(meta_dir.path)
      for x in meta_files:
        m = re.match(r'^epoch_([a-zA-Z0-9]+)\.csv', x)
        if m is not None:
          epoch = meta_dir.add(m.group(0), 'epoch_%s' % m.group(1))
          epoch.is_epoch_table = True
        m = re.match(r'^reference_([a-zA-Z0-9]+)\.csv', x)
        if m is not None:
          ref = meta_dir.add(m.group(0), 'reference_%s' % m.group(1))
          ref.is_reference_table = True
          
    return rave_dir
  
  def is_valid(self, check = ['meta'], print_details = True):
    root_dir = self.directories()
    if not root_dir.is_valid:
      if print_details:
        print('root_dir - %s NOT exists' % root_dir.path)
      return False
    
    if not isinstance(check, list):
      check = list(check)
    if 'meta' in check:
      is_valid = root_dir.get_path('meta/electrodes').is_valid and \
                root_dir.get_path('meta/frequencies').is_valid and \
                root_dir.get_path('meta/time_points').is_valid
      if not is_valid:
        if print_details:
          print('Please check meta directory')
        return False
    
    # TODO: 1. epoch, references, 2. data and preprocess electrodes.h5
    
    return True
    
  
  def load_meta(self, meta, name = None):
    
    if name is None:
      meta_name = meta
    else:
      meta_name = '%s_%s' % (meta, name)
    
    root_dir = self.directories()
    meta_path = root_dir.get_path('meta/%s' % meta_name)
    if meta_path is not None and meta_path.is_valid:
      # csv reader
      if meta == 'epoch':
        return rave_csv_reader(meta_path.path, dtype={
          'Block': np.str,
          'Time' : np.float, 
          'Trial' : np.int32,
          'Condition': np.str,
          'Duration' : np.float
        })
        
      if meta == 'reference':
        return rave_csv_reader(meta_path.path, dtype={
          'Electrode': np.int32,
          'Group' : np.str, 
          'Reference' : np.str,
          'Type': np.str
        }, na_values='')
        
      if meta == 'electrodes':
        return rave_csv_reader(meta_path.path, dtype={
          'Electrode': np.int32,
          'Coord_x' : np.float, 
          'Coord_y' : np.float,
          'Coord_z': np.float,
          'Label' : np.str,
          'Subcortical' : np.bool,
          'SurfaceType' : np.str,
          'Hemisphere' : np.str,
          'VertexNumber' : np.int,
          'IsMini' : np.bool
        })
        
      if meta == 'frequencies':
        return rave_csv_reader(meta_path.path, dtype={
          'Frequency': np.float
        })
        
      if meta == 'time_points':
        return rave_csv_reader(meta_path.path, dtype={
          'Block': np.str,
          'Time' : np.float
        })
      
    return None
  
  
  def get_electrodes(self, reference = None):
    # load electrode table
    electrode_tbl = self.load_meta(meta = 'electrodes', name = None)
    ref_table = self.load_meta(meta = 'reference', name = reference)
    # TODO join two tables
    if ref_table is not None:
      electrode_tbl = pd.merge(electrode_tbl, ref_table, how='left', on=['Electrode'])
    return electrode_tbl

class RAVERepo():
  
  def __init__(self, subject = None):
    if subject is not None:
      self.set_subject(subject)
    else:
      self.subject = None
  
  def set_subject(self, subject):
    if not isinstance(subject, RAVESubject):
      raise Exception('subject must be an instance of RAVESubject')
    self.subject = subject
    
  def get_virtual_dset(self):
    subject_dir = self.subject.directories()
    return os.path.join(subject_dir.get_path('data').path, 'virtual_data.h5')
    
  def _create_virtual_link(self, epoch, pre, post, sample_rate, data_type = 'power'):
    if not self.has_subject:
      raise Exception('No subject set. Please use .set_subject to set subject')
      
    epoch_tbl = self.subject.load_meta('epoch', epoch)
    if epoch_tbl is None:
      raise Exception('Cannot find epoch %s' % epoch)
    epoch_tbl = epoch_tbl.sort_values('Trial')
      
    blocks = np.unique(epoch_tbl['Block'])
    if len(blocks) == 0:
      raise Exception('No block info found in epoch file')
    
    freq_tbl = self.subject.load_meta('frequencies')
    if freq_tbl is None:
      raise Exception('Cannot find frequencies.csv')
      
    # check data path
    subject_dir = self.subject.directories()
    power_dir = subject_dir.get_path('data/%s' % data_type)
    if not power_dir.is_valid:
      raise Exception('Cannot find path %s' % power_dir.path)
      
    h5pattern = os.path.join(power_dir.path, '%d.h5')
    
    electrode_tbl = self.subject.get_electrodes()
    electrodes = list(np.array(electrode_tbl['Electrode']))
    
    # Check electrode files
    power_files = [h5pattern % x for x in electrodes]
    sample_path = power_files[0]
    
    missing_paths = [str(e) for x, e in zip(power_files, electrodes) if not os.path.exists(x)]
    if len(missing_paths):
      raise Exception('Missing files for electrode\n\t%s\nPlease check: %s' % 
                      (', '.join(missing_paths), power_dir.path))
    
    # Figure out dimensions, d2 and d3 will be figured out later
    post_idx = int(np.floor(post * sample_rate))
    pre_idx = int(np.floor(pre * sample_rate))
    d1 = len(epoch_tbl)
    d2 = len(freq_tbl)
    d3 = int(post_idx + pre_idx + 1)
    d4 = len(electrodes)
    
    # Generate virtual dataset
    # electrodes path
    power_files = ['./%s/%d.h5' % (data_type, x) for x in electrodes]
    files = list(zip(electrodes, power_files))
    save_to = self.get_virtual_dset()
    
    # Create placeholder
    layout = h5py.VirtualLayout(
      shape = (d4, d3, d2, d1, ),
      dtype=dtype
    )
    shapes = {}
    with h5py.File(sample_path, 'r') as f:
      for block in blocks:
        entry_key = '/raw/%s/%s' % (data_type, block)
        shapes[block] = f[entry_key].shape
    
    # Virtual dataset doesn't require data existence
    for i, file_dup in enumerate(files):
      filename = file_dup[1]
      el = file_dup[0]
      
      # splice by epochs
      for ii, t in enumerate(epoch_tbl.Time):
        start_idx = int(np.round(t * sample_rate) - pre_idx)
        end_idx = int(np.round(t * sample_rate) + post_idx + 1)
        block = epoch_tbl.Block[ii]
        entry_key = '/raw/%s/%s' % (data_type, block)
        vsource = h5py.VirtualSource(filename, entry_key, shape = shapes[block])
        layout[i,:,:,ii] = vsource[start_idx:end_idx:1,]
        
    # Write as virtual data
    with h5py.File(save_to, 'a', libver='latest') as f:
      f.create_virtual_dataset('/%s/%s' % (epoch, data_type), layout)
      
    pass
    
  def generate_virtual_link(self, data_types = ['power', 'phase', 'voltage']):
    # Get all epochs
    subject_dir = self.subject.directories()
    meta_dir = subject_dir.get_path('meta')
    epochs = [x for x in meta_dir.keys() if re.match(r'^epoch_', x) is not None]
    
    to_save = self.get_virtual_dset()
    if os.path.exists(to_save):
      os.unlink(to_save)
    
    for epoch in epochs:
      epoch = re.match(r'^epoch_(.*)', epoch).group(1)
      for dt in data_types:
        try:
          self._create_virtual_link(epoch = epoch, data_type=dt)
        except Exception as e:
          print('Failed in generating link for data type [%s] (%s)' % (dt, epoch))
      
    pass
  
  @property
  def has_subject(self):
    if isinstance(self.subject, RAVESubject):
      return True
    return False
  
  def epoch_electrodes(self, data_type, reference, epoch, pre, post, sample_rate, save_to):
    # reference = 'default'
    # epoch = 'YABa'
    # data_type = 'power'
    # electrodes = range(1,10)
    subject_dir = self.subject.directories()
    cache_dir = subject_dir.get_path('data/cache/cached_reference')
    power_dir = subject_dir.get_path('data/power')
    phase_dir = subject_dir.get_path('data/phase')
    voltage_dir = subject_dir.get_path('data/voltage')
    
    tmp_dir = subject_dir.get_path('data/%s' % data_type)
    if not tmp_dir.is_valid:
      raise Exception('Cannot find %s path - %s' % (data_type, tmp_dir.path))
    
    if cache_dir.is_valid:
      cache_tbl = rave_csv_reader(cache_dir.path, dtype={
        'Electrode' : np.int,
        'Reference' : np.str
      })
    else:
      cache_tbl = None
    
    # Get epoch
    epoch_tbl = self.subject.load_meta('epoch', epoch)
    if epoch_tbl is None:
      raise Exception('Cannot find epoch %s' % epoch)
    epoch_tbl = epoch_tbl.sort_values('Trial')
    blocks = np.unique(epoch_tbl.Block)
    if len(blocks) == 0:
      raise Exception('Cannot find any blocks in epoch %s' % epoch)
        
    # Get reference
    ref_tbl = self.subject.load_meta('reference', reference)
    if ref_tbl is None:
      raise Exception('Cannot find reference %s' % reference)
    # get electrodes
    ref_tbl = self.subject.get_electrodes(reference)
    
    # get frequency
    freq_tbl = self.subject.load_meta('frequencies')
    if freq_tbl is None:
      raise Exception('Cannot find frequencies.csv')
    
    # Check Cache!
    # Get dimensions
    d1 = len(epoch_tbl)
    d2 = len(freq_tbl)
    pre_idx = int(np.floor(pre * sample_rate))
    post_idx = int(np.floor(post * sample_rate))
    d3 = pre_idx + post_idx + 1
    d4 = sum(ref_tbl.Reference.notna())
    
    valid_electrodes = ref_tbl.Electrode[ref_tbl.Reference.notna()]
    valid_electrodes = np.array(valid_electrodes.to_list())
    
    try:
      os.unlink(save_to)
    except Exception as e:
      pass
    with h5py.File(save_to, 'w') as target_file:
      target_file.create_dataset(
        name=data_type, 
        shape=(d4,d3,d2,d1,), 
        dtype='f8', 
        chunks = (1, d3, d2, 1, ))
      # target_dset = target_file[data_type]
      
    # get cached table
    for e in valid_electrodes:
      print(e)
      d4_idx = np.where(valid_electrodes == e)
      d4_idx = d4_idx[0][0]
      # get reference name
      ref_name = ref_tbl.Reference[ref_tbl.Electrode == e]
      ref_name = ref_name[ref_name.index[0]]
      
      elec_path = os.path.join(tmp_dir.path, '%d.h5' % e)
      
      if cache_tbl is not None:
        cache_name = cache_tbl.Reference[cache_tbl.Electrode == e]
        cache_name = cache_name[cache_name.index[0]]
      else:
        with h5py.File(elec_path, 'r') as f:
          cache_name = f['/reference'][:]
          cache_name = cache_name.tolist()[0]
          cache_name = cache_name.decode()
      
      if ref_name == cache_name:
        # Load from elec_path /ref/%s/block
        with h5py.File(save_to, 'a') as target_file:
          with h5py.File(elec_path, 'r') as f:
            for i, t in enumerate(epoch_tbl.Time):
              print(i)
              block = epoch_tbl.Block[i]
              time_pt = int(np.round(t * sample_rate))
              d = f['/ref/%s/%s' % (data_type, block)][(time_pt-pre_idx):(1 + post_idx + time_pt):1,]
              target_file[data_type][d4_idx, :, :, i] = d
        pass
          


root_dir = '/Volumes/ATGC/rave_data/data_dir/'
project_name = 'congruency'
subject_code = 'YAB'
subject = RAVESubject(project_name, subject_code, root_dir)
subject_dir = subject.directories()
repo = RAVERepo(subject)

t1 = datetime.datetime.now()
repo.epoch_electrodes(data_type='power', reference='default', epoch='YABa', pre=1, post=2, sample_rate=100, save_to='/Volumes/ATGC/rave_data/data_dir/congruency/YAB/rave/data/virtual_data.h5')
t2 = datetime.datetime.now()
t2-t1

save_to = '/Volumes/ATGC/rave_data/data_dir/congruency/YAB/rave/data/virtual_data.h5'


save_to = os.path.join(subject_dir.get_path('data').path, 'virtual_data.h5')
repo.generate_virtual_link()

epoch = 'YABa'
entry_key = '/raw/phase/008'
f = h5py.File(save_to, 'r', libver='latest')
dd = f['/%s%s' % (epoch, entry_key)][1:10,:,:]


