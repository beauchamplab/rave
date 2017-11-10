function [ s1, s2, s3] = get_covariance_array_normal(X, varargin)
% GET_COVARIANCE_ARRAY_NORMAL     [ s1, s2, s3 ] = get_covariance_array_normal(X)
%
% This function estimates the array normal covariance, assuming that X
% follows an array normal distribution.
%
% INPUT:
%   1. X: Initial data is 4-dimensional, (trials x electrode x freq x
%   time), class 'double' or 'tensor'
%   2.verbose (default true): Optional, boolean value indicating to print
%   all messages in code
%
% OUTPUT:
%   1. s1:
%   2. s2:
%   3. s3:

% Specify default parameters and their expected values
default_verbose = true;
% Parse and check input parameters
p = inputParser;
p.CaseSensitive = true;
addRequired(p, 'X', @(i) isa(i, 'double')||isa(i, 'tensor'));
addOptional(p, 'verbose', default_verbose, @islogical);
parse(p, X, varargin{:});
verbose = p.Results.verbose;
% Make X a double
if isa(X, 'tensor'); X = double(X); end

%initialize variables
[n_cov, row_cov, col_cov, dep_cov] = size(X);
epsilon = .001;
    
s1 = diag(ones(1,row_cov));
s2 = diag(ones(1,col_cov));
s3 = diag(ones(1,dep_cov));
S_1 = diag(s1);
S_2 = diag(s2);
S_3 = diag(s3);
    
tol = 1e-4;
% tol = .1;
maxit = 1000;
err = 1;
     
% iterate
for i = 1:maxit
   
    if err < tol
        break;
    end
    
    old_s1 = s1;
    old_s2 = s2;
    old_s3 = s3;

    E_tilde_k = tenmat(ttm(ttm(tensor(X),s2,3),s3,4 ),2);
    
    S_1 = cellfun(@(x) x'*x,num2cell(E_tilde_k.data',1))/(n_cov*col_cov*dep_cov);
    s1 = diag(1./sqrt(S_1));

    E_tilde_k = tenmat(ttm(ttm(tensor(X),s1,2),s3,4 ),3);
    S_2 = cellfun(@(x) x'*x,num2cell(E_tilde_k.data',1))/(n_cov*row_cov*dep_cov);
    S_3 = dep_cov*S_3/sum(S_3);
    s2 = diag(1./sqrt(S_2));

    E_tilde_k = tenmat(ttm(ttm(tensor(X),s1,2),s2,3 ),4);
    S_3 = cellfun(@(x) x'*x,num2cell(E_tilde_k.data',1))/(n_cov*col_cov*row_cov);
    S_3 = dep_cov*S_3/sum(S_3);
    s3 = diag(1./sqrt(S_3));
    
    err1 = norm(s1-old_s1);
    err2 = norm(s2-old_s2);
    err3 = norm(s3-old_s3);
    err = err1 + err2 + err3;
    
    if verbose; fprintf('Array normal covariance estimation error: %f \n', err); end
end
    
   % S_1 = S_1 + diag(epsilon);
   % S_2 = S_2 + diag(epsilon);
   % S_3 = S_3 + diag(epsilon);
end
