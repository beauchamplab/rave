function [] = cmd_process_patient_data(fn)
% This function is a command line interface to process patient data.
% Capable of implementing both standardization and outlier removal, in
% accordance to incomming parameters.
%
% INPUT 
%   1. fn: The '.h5' file containing parameters

% Parse parameters
p = inputParser;
p.CaseSensitive = true;
addRequired(p, 'fn', @ischar);
parse(p, fn);

% Check file names
fn = deblank(strtrim(fn));
[~, ~, in_ext] = fileparts(fn); 
if ~strcmp(in_ext, '.h5'); error('The file %s must be a .h5 file', fn); end
if exist(fn, 'file') ~= 2; error('The file %s does not exist', fn); end

% Set up
rhopls_setup();

% Load data into Matlab and configure it
data = h5read(fn, '/ecog/X');
v = logical(h5read(fn, '/params/v'));
maxIterations = h5read(fn, '/params/maxIterations');
%saveOutputAs = h5read(fn, '/params/saveOutputAs');
%saveOutputAs = strtrim(deblank(saveOutputAs{1}));
saveOutputAs = '';
standardizationMethod = h5read(fn, '/params/standardizationMethod');
standardizationMethod = strtrim(deblank(standardizationMethod{1}));
center = logical(h5read(fn, '/params/center'));
scaled = logical(h5read(fn, '/params/scaled'));
removeOutliers = logical(h5read(fn, '/params/removeOutliers'));

% Permute X: (trial x freq x tim x elec) --> (trial x elec x freq x tim)
data = permute(data, [1 4 2 3]);

% Process and save
[X, outliers] = process_patient_data(data, 'verbose', v, ...
    'maxIterations', maxIterations, ...
    'saveOutputAs', saveOutputAs, ...
    'standardizationMethod', standardizationMethod, ...
    'center', center, 'scaled', scaled, ...
    'removeOutliers', removeOutliers);
X = double(X);
if length(outliers) < 1; outliers = [0]; end
outliers = int8(outliers);

% Permute X:  (trial x elec x freq x tim) --> (trial x freq x tim x elec)
X = permute(X, [1 3 4 2]);

% Save results to hdf5 file
h5create(fn, '/out/outX', size(X)); 
h5write(fn, '/out/outX', X);
h5create(fn, '/out/outliers', size(outliers)); 
h5write(fn, '/out/outliers', outliers);

% Exit matlab program
exit()

end
