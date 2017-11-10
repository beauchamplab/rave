function [X, outliers] = process_patient_data(data, varargin)
%
% Creates the covariates X for a single patient, which is an 4-dimensional 
% tensor with dimensions (trials x freq x time electrode). Users can 
% specify preprocessing options, as  well a remove outliers from the data. 
%
% INPUT:
%   1. data: Design tensor, akin to design matrix, with dimensions 
%   (trials x freq x time electrode). 
%
%   General options
%   2. verbose (default true): Optional, boolean valye indicating to print
%   all messages in code
%   3. maxIterations (default 1000): Optional, maximum number of iterations
%   for outlier detection
%   4. saveOutputAs (default ''): Optional, name of '.mat' file to save
%   results. If '' entered then no data will be saved.
%
%   Options for standardization
%   5. standardizationMethod (default 'none'): Optional, standardization
%   options 'none', 'flatten_trials', and 'array_normal'
%   6. center (default true): Optional, boolean value indicating to
%   mean-center data
%   7. scaled (default true): Optional, boolean value indicating scaling by
%   standard deviation

%   Options for removing outliers
%   8. removeOutliers (default true): Optional, boolean value indicating to
%   remove outliers from data

%
% OUTPUT:
%   1. X: ECoG data, with pre-processing as specified by input values
%   2. Outliers: List of trials identified as outliers
%
% See also standardize_data.m, and automated_outliers.m

% Specify default parameters and their expected values
default_standardizationMethod = 'none';
expected_standardizationMethod = {default_standardizationMethod, 'flatten_trials', 'array_normal'};
default_center = true;
default_scaled = true;
default_removeOutliers = false;
default_verbose = true;
default_maxIterations = 1000;
default_saveOutputAs = '';
% Parse and check input parameters
p = inputParser;
p.CaseSensitive = true;
addRequired(p, 'data', @(ii) isa(ii,'numeric')||isa(ii,'tensor'));
addOptional(p, 'standardizationMethod', default_standardizationMethod, @(ii) any(validatestring(ii, expected_standardizationMethod)));
addOptional(p, 'center', default_center, @islogical);
addOptional(p, 'scaled', default_scaled, @islogical);
addOptional(p, 'removeOutliers', default_removeOutliers, @islogical);
addOptional(p, 'verbose', default_verbose, @islogical);
addOptional(p, 'maxIterations', default_maxIterations, @(ii) isnumeric(ii)&&(ii>2));
addOptional(p, 'saveOutputAs', default_saveOutputAs, @ischar);
% parse(p, data, trial_labels, varargin{:});
parse(p, data, varargin{:});
standardizationMethod = p.Results.standardizationMethod;
center = p.Results.center;
scaled = p.Results.scaled;
removeOutliers = p.Results.removeOutliers;
verbose = p.Results.verbose;
max_it = p.Results.maxIterations;
output_fn = p.Results.saveOutputAs;

% Check Input
% data = squeeze(data);       % Remove singleton dimensions, if present
if length(size(data)) ~= 4
    error('Data must be a 4-dimensional tensor with dimensions (trials x electrode x freq x time)');
end

output_fn = deblank(strtrim(output_fn));
if ~strcmp(output_fn, default_saveOutputAs)
    % Is this a .mat file?
    [~, ~, ext] = fileparts(output_fn); 
    if ~strcmp(ext, '.mat'); error('The filename must be a .mat file'); end
end

% Permute X for new dimensions are (trials x freq x time electrode)
data = permute(data, [1 3 4 2]);

% Standardize Data
X = data;
if ~strcmp(standardizationMethod, 'none') 
    X = standardize_data(data, ...
        'standardizationMethod', standardizationMethod, ...
        'center', center, 'scale', scaled, 'verbose', verbose);
end
if ~isa(X, 'tensor'); X = tensor(X); end
    
% Identify and remove outliers
outliers = [];
if removeOutliers
    % cd to code dir
    original_dir = pwd;          % Save original dir
    fp = mfilename('fullpath');
    [pathstr, ~, ~] = fileparts(fp);
    cd(pathstr); addpath(pathstr); savepath;
    % Identify outliers
    outliers = automated_outliers(X, 'verbose', verbose, 'maxIterations', max_it);
    % Remove outliers from data
    nTrials = size(data,1);
    index = 1:nTrials;
    index = setdiff(index, outliers);
    X = data(index,:,:,:);
    % Re-standardize data after removing outliers
    if ~strcmp(standardizationMethod, 'none')
        X = standardize_data(X, ...
            'standardizationMethod', standardizationMethod, ...
            'center', center, 'scale', scaled, 'verbose', verbose);
    end
    cd(original_dir);
end

% Save processed data, outliers, and metadata
if ~strcmp(output_fn, '')
    % Create metadata
    keySet = {'standardizationMethod', 'center', 'scaled', 'removeOutliers', 'maxIterations'};
    valueSet = {standardizationMethod, center, scaled, removeOutliers, max_it};
    metadata = containers.Map(keySet, valueSet);
    % Save everything
    save(output_fn, 'X', 'outliers', 'metadata', '-v6');
end

end
 