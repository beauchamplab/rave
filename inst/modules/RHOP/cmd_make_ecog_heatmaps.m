function [] = cmd_make_ecog_heatmaps(fn)
% This function is a command line interface to create all possible HOPCA
% heatmaps
%
% INPUT 
%   1. fn: The '.h5' file containing parameters

% Parse parameters
p = inputParser;
p.CaseSensitive = true;
addRequired(p, 'fn', @ischar);
parse(p, fn);

% Check file names
fn = strtrim(fn);
[~, ~, in_ext] = fileparts(fn); 
if ~strcmp(in_ext, '.h5'); error('The file %s must be a .h5 file', fn); end
if exist(fn, 'file') ~= 2; error('The file %s does not exist', fn); end

% Set up
rhopls_setup();

% Load data into Matlab and configure it
X = h5read(fn, '/ecog/X');
U = h5read(fn, '/params/U');
V = h5read(fn, '/params/V');
W = h5read(fn, '/params/W');
Ti = h5read(fn, '/params/Ti');
%saveOutputAs = h5read(fn, '/params/saveOutputAs');
%saveOutputAs = strtrim(deblank(saveOutputAs{1}));
saveOutputAs = '';
K = size(U,2);
X = tensor(X);

% Permute X: (trial x freq x tim x elec) --> (trial x elec x freq x tim)
X = permute(X, [1 4 2 3]);

% Check input parameters
% Check X
if length(size(X)) ~= 4; error('X must be a 4-dimensional tensor'); end
% Check K
all_k = [size(U,2), size(V,2), size(W,2), size(Ti,2)];
if ~all(all_k == K); error('All factors must have the same component dimension'); end
maxK = min(size(X))-1;
if K > maxK; error('The number of components cannot exceed %d', maxK); end
% check U, V, W, Ti
if size(U,1) ~= size(X,1); error('Dimension mismatch in trials'); end
if size(V,1) ~= size(X,2); error('Dimension mismatch in electrodes'); end
if size(W,1) ~= size(X,3); error('Dimension mismatch in frequency'); end
if size(Ti,1) ~= size(X,4); error('Dimension mismatch in time'); end

% Create and store results
trial_x_electrode = cell(K,1);
trial_x_frequency = cell(K,1);
trial_x_time = cell(K,1);
electrode_x_frequency = cell(K,1);
electrode_x_time = cell(K,1);
frequency_x_time = cell(K,1);
for k = 1:K
    % trials x electrodes
    trial_x_electrode{k} = double(ttv(ttv(X, W(:,k), 3), Ti(:,k), 3));
    % trials x frequencies
    trial_x_frequency{k} = double(ttv(ttv(X, V(:,k), 2), Ti(:,k), 3));
    % trials x time
    trial_x_time{k} = double(ttv(ttv(X, V(:,k), 2), W(:,k), 2));
    % electrodes x frequencies
    electrode_x_frequency{k} = double(ttv(ttv(X, U(:,k), 1), Ti(:,k), 3));
    % electrodes x time
    electrode_x_time{k} = double(ttv(ttv(X,U(:,k),1), W(:,k), 2));
    % frequencies x time
    frequency_x_time{k} = double(ttv(ttv(X, U(:,k),1), V(:,k), 1));
end
    
% Save results to hdf5 file
for k = 1:K
    tr_x_e_name = sprintf('/out/tr_x_e_%d', k);
    h5create(fn, tr_x_e_name, size(trial_x_electrode{k}));
    h5write(fn, tr_x_e_name, trial_x_electrode{k});
    tr_x_f_name = sprintf('/out/tr_x_f_%d', k);
    h5create(fn, tr_x_f_name, size(trial_x_frequency{k}));
    h5write(fn, tr_x_f_name, trial_x_frequency{k});
    tr_x_t_name = sprintf('/out/tr_x_t_%d', k);
    h5create(fn, tr_x_t_name, size(trial_x_time{k}));
    h5write(fn, tr_x_t_name, trial_x_time{k});
    e_x_f_name = sprintf('/out/e_x_f_%d', k);
    h5create(fn, e_x_f_name, size(electrode_x_frequency{k}));
    h5write(fn, e_x_f_name, electrode_x_frequency{k});
    e_x_t_name = sprintf('/out/e_x_t_%d', k);
    h5create(fn, e_x_t_name, size(electrode_x_time{k}));
    h5write(fn, e_x_t_name, electrode_x_time{k});
    f_x_t_name = sprintf('/out/f_x_t_%d', k);
    h5create(fn, f_x_t_name, size(frequency_x_time{k}));
    h5write(fn, f_x_t_name, frequency_x_time{k});
end

% Write to file, if specified
if ~strcmp(saveOutputAs, '')
    save(saveOutputAs, 'trial_x_electrode', 'trial_x_frequency', ...
        'trial_x_time', 'electrode_x_frequency', 'electrode_x_time', ...
        'frequency_x_time', '-v6');
end

% Exit matlab program
exit()

end
