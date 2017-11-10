function [ return_tensor ] = center_scale_array_normal( X, center, scaled, varargin)
% CENTER_SCALE_ARRAY_NORMAL [return_tensor] = center_scale_array_normal(X, center, scale)
%
% This function  centers and scales data using the array normal method, and
% according by user options. 
%
% The array normal distribution is a higher-order generalization of the
% multivariate normal distribution. We estimate a mean and covariance
% assuming the tensor was generated via the array normal distribution and
% use the estimated parameters to center and scale the tensor.
%
% INPUT:
%   1. X: Initial 4-dimensional data tensor (trials x electrode x freq x
%   time), class 'tensor' or 'double'
%   2. center: Boolean value indicating to mean-center data
%   3. scaled: Boolean value indicating scaling by standard deviation
%   4. verbose (default true): Optional, boolean value indicating to print
%   all messages in code
%
% OUTPUT:
%   1. return_tensor: scaled data, class 'tensor'
%
% See get_mean_array_normal.m, and get_covariance_array_normal.m

% Specify default parameters and their expected values
default_verbose = true;
% Parse and check input parameters
p = inputParser;
p.CaseSensitive = true;
addRequired(p, 'X', @(x) isa(x,'tensor')||isa(x,'double'));
addRequired(p, 'center', @islogical);
addRequired(p, 'scaled', @islogical);
addOptional(p, 'verbose', default_verbose, @islogical);
parse(p, X, center, scaled, varargin{:});
verbose = p.Results.verbose;
% Convert X to double
if isa(X, 'double'); X = tensor(X); end

% Get dimensions
ns = size(X);
n = ns(1);
p = ns(2);
q = ns(3);
t = ns(4);
return_tensor = X;

if center ~= 0
    [mu_p, mu_q, mu_t] = get_mean_array_normal(X);
    M_p = zeros(p,q,t);
    M_q = zeros(p,q,t);
    M_t = zeros(p,q,t);
    X = tensor(X);

    for i = 1:p
        M_p(i,:,:) =  ones(q,t)*mu_p(i);
    end

    for i = 1:q
        M_q(:,i,:) =  ones(p,t)*mu_q(i);
    end

    for i = 1:t
        M_t(:,:,i) =  ones(p,q)*mu_t(i);
    end

    for i = 1:n
        M(i,:,:,:) = M_p + M_q + M_t;
    end
    return_tensor = X - M;
end

if scaled ~= 0
    [Sigma_p, Sigma_q, Sigma_t] = get_covariance_array_normal(return_tensor, 'verbose', verbose);
    return_tensor = ttm( ttm( ttm(tensor(return_tensor),Sigma_p,2),Sigma_q,3),Sigma_t,4);
end
end