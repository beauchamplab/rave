function [U, V, W, T, D, Xhat, objVals] = hopca_cptpa_ecog(x, K, varargin)
% HOPCA_CPTPA_ECOG
%
% Computes the K sparse higher order orthogonal iteration factors
%
% INPUT:
%   1. x: a data matrix with dimensions (trials x electrode x freq x time)
%   2. K: the number of components, K > 0
%   3. lamu (default 0): Optional, soft thresholding parameter for trials
%   4. lamv (default 0): Optional, soft thresholding parameter for electrodes
%   5. lamw (default 0): Optional, soft thresholding for frequencies
%   6. omega: Optional, tim x tim positive semi-definite matrix for
%   smoothing penalty over time
%   7. maxIterations (default 1000): Optional, maximum number of alternating
%   regressions steps
%   8. verbose (default true): Optional, boolean value indicating to print
%   all messages in code
%   9. saveOutputAs (default ''): Optional, results will be saved to 
%   '.mat' file specified. Otherwise results will not be saved. 
%   10. saveVersion (default '-v7.3'): Optional, specifices version of save
%   as either '-v6' or '-v7.3'
%
% OUTPUT:
%   1. U: (trails x K) the observation factor
%   2. V: (electrodes x K) the node factor
%   3. W: (frq x K) the frequency factor
%   4. T: (time x K) the time factor
%   5. D: (K x 1) vector of scale parameters
%   6. Xhat: (trials x electrode x freq x time) deflated tensor (tensor 
%      with the effect of the estimated factors removed
%   7. objVals: a vector of objective function values
%
% See soft_thr.m

% Specify default parameters and their expected values
default_lamu = 0;
default_lamv = 0;
default_lamw = 0;
default_maxit = 1000;
default_omega = [];
default_saveVersion = '-v7.3';
expected_saveVersion = {default_saveVersion, '-v6'};
% default_omega = gallery('tridiag', 1);
default_verbose = true;
default_saveOutputAs = '';
% Parse input parameters
p = inputParser;
p.CaseSensitive = true;
addRequired(p, 'x', @(ii) isa(ii,'double')||isa(ii,'tensor'));
addRequired(p, 'K', @(ii) (isnumeric(ii)&&(ii > 0)));
addOptional(p, 'lamu', default_lamu, @(ii) isnumeric(ii)&&(ii >= 0));
addOptional(p, 'lamv', default_lamv, @(ii) isnumeric(ii)&&(ii >= 0));
addOptional(p, 'lamw', default_lamw, @(ii) isnumeric(ii)&&(ii >= 0));
addOptional(p, 'maxIterations', default_maxit, @(ii) isnumeric(ii)&&(ii>1));
addOptional(p, 'omega', default_omega, @(ii) (isa(ii,'double')||isa(ii,'tensor')||isempty(ii)));
addOptional(p, 'verbose', default_verbose, @islogical);
addOptional(p, 'saveOutputAs', default_saveOutputAs, @ischar);
addOptional(p, 'saveVersion', default_saveVersion, @(ii) any(validatestring(ii,expected_saveVersion)));
parse(p, x, K, varargin{:});
lamu = p.Results.lamu;
lamv = p.Results.lamv;
lamw = p.Results.lamw;
maxit = p.Results.maxIterations;
omega = p.Results.omega;
verbose = p.Results.verbose;
output_fn = p.Results.saveOutputAs;
saveVersion = p.Results.saveVersion;

% Check parameters
% x = squeeze(x);         % Remove singletion dimensions, if present
if ~isa(x, 'tensor'); x = tensor(x); end
% if length(size(X)) == 3
%     % Here we assume that this is called for hopls where the covariance
%     % tensor has dimensions
% end
if length(size(x)) ~= 4
    error('Data must be a 4-dimensional tensor with dimensions (trials x electrode x freq x time)');
end
ns = size(x);
n = ns(1); p = ns(2); q = ns(3); tim = ns(4);
if isempty(omega); omega = gallery('tridiag', tim); end
% if max(size(omega)) < 2; omega = gallery('tridiag', tim); end
if (length(size(omega)) ~= 2) || (size(omega,1) ~= size(omega,2)) || (size(omega,1) ~= tim)
    error('Omega must be a matrix with dimension (time x time)');
end
if isa(omega,'tensor'); omega = double(omega); end
maxK = min(size(x))-1;
if K > maxK; error('K must be between 1 and %d', maxK); end
output_fn = deblank(strtrim(output_fn));
if ~strcmp(output_fn, default_saveOutputAs)
    % Is this a .mat file?
    [~, ~, ext] = fileparts(output_fn); 
    if ~strcmp(ext, '.mat'); error('The filename must be a .mat file'); end
end

% Create CP components for each factor
U = zeros(n, K);        % trails
V = zeros(p, K);        % electrodes
W = zeros(q, K);        % frequencies
T = zeros(tim, K);      % time
D = zeros(K, 1);        % scaling factor
Xhat = x;               % initialize Xhat
% objVals = zeros(K,1);   % Record objective values
% objVals = cell(K,1);

% Initialize random CP components with norm of 1
u = randn(n,1); u = u/norm(u);
v = randn(p,1); v = v/norm(v);
w = randn(q,1); w = w/norm(w);
t = randn(tim,1); t = t/norm(t);
d = randn(1,1);

for k = 1:K
    if verbose; fprintf('Computing factor %d\n', k); end
    obj = u'*double(ttv(ttv(Xhat,v,2),w,2))*t - lamu*sum(abs(u)) - lamv*sum(abs(v))-lamw*sum(abs(w));
	ind_vec = obj;
    iter = 0; thr = 1e-6; ind = thr + 1;
    while (ind > thr) && (maxit > iter)
        oldu = u; oldv = v; oldw = w; oldt = t; old_obj = obj(end); 
        % trials --> L1 penalty
        uhat = soft_thr(double(ttv(ttv(Xhat,v,2),w,2))*t, lamu, 0);
        if norm(uhat)==0
            u = zeros(n,1);
        else
            u = uhat/norm(uhat,2);
        end
        % electrodes --> L1 penalty
        vhat = soft_thr(double(ttv(ttv(Xhat,u,1),w,2))*t, lamv, 0);
%         vhat = wthresh(double(ttv(ttv(Xhat,u,1),w,2))*t, 'h', lamv);
        if norm(vhat) == 0
            v = zeros(p,1);
        else
            v = vhat/norm(vhat,2);
        end
        % frequencies --> L1 penalty
        what = soft_thr(double(ttv(ttv(Xhat,u,1),v,1))*t, lamw, 0);
        if norm(what)==0
            w = zeros(q,1);
        else
            w = what/norm(what,2);
        end
       % smooth by time
       S = (eye(tim)+omega);
       that = S\double(ttv(ttv(Xhat,u,1),v,1))'*w;
       if norm(that)==0
            t = zeros(tim,1);
       else
           %t = that/norm(that,2);
            t = that/sqrt(that'*S*that);
       end
       % Compute objective value
       % TODO = fix norms
       obj = [obj; u'*double(ttv(ttv(Xhat,v,2),w,2))*t - lamu*sum(abs(u)) - lamv*sum(abs(v))- lamw*sum(abs(w))];
       % Record convergence errors
       ind = abs((obj(end) - old_obj)/obj(1));
       if verbose; fprintf('\tConvergence error: %f\n', ind); end
       ind_vec = [ind_vec; obj];
       iter = iter + 1;
    end
%     objVals{k} = ind_vec;
    % Record objective values
    objVals = ind_vec;              
    % Compute CP-scaling constant for factor k
    d = u'*double(ttv(ttv(Xhat,v,2),w,2))*t;
    % Deflate tensor
    Xhat = Xhat - full(ktensor(d,u,v,w,t));
    % Store components for factor k
    U(:,k) = u;
    V(:,k) = v;
    W(:,k) = w;
    T(:,k) = t;
    D(k) = d;
end

if ~strcmp(output_fn, '')
    % Create metadata
    keySet = {'K', 'lamu', 'lamv', 'lamw', 'omega', 'maxIterations'};
    valueSet = {K, lamu, lamv, lamw, omega, maxit};
    metadata = containers.Map(keySet, valueSet);
    % Save everything
    save(output_fn, 'U', 'V', 'W', 'T', 'D', 'Xhat', 'objVals', 'metadata', saveVersion);
end

end



