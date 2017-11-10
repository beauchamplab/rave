 function [ return_matrix ] = center_scale(X, varargin)
% CENTER_SCALE [return_matrix] = center_scale(X, center, scaled)
%
% This function flattens the ECoG tensor by trial, or first dimension, and 
% then centers and scales data according by user options
%
% INPUT:
%   1. X: Initial 2-dimensional data matrix
%   2. center (default true): Boolean value indicating to mean-center data
%   3. scaled (default true): Boolean value indicating scaling by standard deviation
%
% OUTPUT:
%   1. return_matrix: scaled data

% Specify default parameters and their expected values
default_center = true;
default_scaled = true;
% Parse input parameters
p = inputParser;
p.CaseSensitive = true;
addRequired(p, 'X', @(x) isa(x,'double'));
addOptional(p, 'center', default_center, @islogical);
addOptional(p, 'scaled', default_scaled, @islogical);
parse(p, X, varargin{:});
center = p.Results.center;
scaled = p.Results.scaled;

% Check that X is a matrix
if length(size(X)) ~= 2; error('X must be a matrix or vector'); end

% Initialize
ns = size(X);
n = ns(1);
P = eye(n) - (1/n)*ones(n);

if center == true && scaled == true
    return_matrix = bsxfun(@rdivide,P*X,sqrt(var(X)));
end

if center == true && scaled == false
    return_matrix = P*X;
end

if center == false && scaled == true
    return_matrix = bsxfun(@rdivide,X,sqrt(var(X)));
end

if center == false && scaled == false
    return_matrix = X;
end

end