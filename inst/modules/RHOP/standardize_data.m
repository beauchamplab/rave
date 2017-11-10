function [sdX] = standardize_data(X, varargin)
% LOAD_PATIENT_DATA  [sdX] = standardize_data(X, 
%                               standardization_method,     
%                               center,                     
%                               scaled,                 
%                               verbose)
%
%   This function standardizes data X, by user specifications
% 
%   Ex. [sdX] = standardize_data(X, ...)
%
% INPUT:
%   2. standardizationMethod (default 'center_scale'): Optional, standardization
%   options 'center_scale', 'flatten_trials', and 'array_normal'
%   3. center (default true): Optional, boolean value indicating to
%   mean-center data
%   4. scaled (default true): Optional, boolean value indicating scaling by
%   standard deviation
%   7. verbose (default true): Optional, boolean valye indicating to print
%   all messages in code
%
% OUTPUT:
%   1. stdX: standardized preprocessing data
%
% See also center_scale.m, center_scale_array_normal.m

% Specify default parameters and their expected values
default_std_method = 'center_scale';
expected_std_methods = {default_std_method, 'flatten_trials', 'array_normal'};
default_center = true;
default_scale = true;
default_verbose = true;
% Parse and check input parameters
p = inputParser;
p.CaseSensitive = true;
addRequired(p, 'X', @(x) isa(x,'tensor')||isa(x,'double'));
addOptional(p, 'standardizationMethod', default_std_method, @(x) any(validatestring(x,expected_std_methods)));
addOptional(p, 'center', default_center, @islogical);
addOptional(p, 'scaled', default_scale, @islogical);
addOptional(p, 'verbose', default_verbose, @islogical);
parse(p, X, varargin{:});
std_method = p.Results.standardizationMethod;
center = p.Results.center;
scaled = p.Results.scaled;
verbose = p.Results.verbose;

ndim = length(size(X));
if (strcmp(std_method, 'flatten_trials') || strcmp(std_method, 'array_normal')) && (ndim < 3)
    error('The standardization method %s is for tensors only. Input X has %s dimensions', std_method, char(ndim));
end

if strcmp(std_method, 'center_scale') && (ndim > 2)
    error('The standardization method %s is for tensors only. Input X has %s dimensions', std_method, char(ndim));
end

% Center and scale data
switch std_method
    case expected_std_methods{1}
        fprintf('Standardize by simple centering and scaling\n');
        sdX = center_scale(X, 'center', center, 'scaled', scaled);
    case expected_std_methods{2}
        fprintf('Standardize by flattening by trial\n')
        tmp = tenmat(tensor(X),1);
        tmp(:,:) = center_scale(tmp.data, 'center', center, 'scaled', scaled);     
        sdX = tensor(tmp);
    case expected_std_methods{3}
        fprintf('Standardize by array normal distribution assumption\n');
        sdX = center_scale_array_normal(X, center, scaled, 'verbose', verbose);
        if ~isa(sdX, 'tensor'); sdX = tensor(sdX); end
    otherwise
        error('The standardization method %s is not recognized', std_method);
end

end