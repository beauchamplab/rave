function [ outliers] = automated_outliers( X, varargin )
% AUTOMATED_OUTLIERS    [ outliers] = automated_outliers( X )
%
% This function identifies outlier trails in X
%
% INPUT:
%   1. X: Initial data containing outliers is 4-dimensional, (trials x
%   electrode x freq x time), class 'tensor' or 'double'
%   2.verbose (default true): Optional, boolean value indicating to print
%   all messages in code
%   3. maxIterations (default 1000): Optional, maximum number of iterations
%
% OUTPUT:
%   1. outliers: Vector out outlier idexes

% Specify default parameters and their expected values
default_verbose = true;
default_maxIterations = 1000;
% Parse and check input parameters
p = inputParser;
p.CaseSensitive = true;
addRequired(p, 'X', @(i) isa(i, 'double')||isa(i, 'tensor'));
addOptional(p, 'verbose', default_verbose, @islogical);
addOptional(p, 'maxIterations', default_maxIterations, @(x) isnumeric(x)&&(x>1));
parse(p, X, varargin{:});
verbose = p.Results.verbose;
max_it = p.Results.maxIterations;
% Make X a double
if isa(X, 'double'); X = tensor(X); end

K = 0;
outliers = [];
ns = size(X);
obs = ns(1);
nds = ns(2);
frq = ns(3);
tim = ns(4);
ind = 1:obs;
iter = 0;
current_max = 0;

%  current_max = obs;
U = []; V = []; W = []; T=[]; D = [];
Xhat = X;
ns = size(X);
n = ns(1); p = ns(2); q = ns(3); tim = ns(4);

u = randn(n,1);
%u = u/norm(u);
v = randn(p,1);
%v = v/norm(v);
w = randn(q,1);
%w = w/norm(w);
t = randn(tim,1);
%t = t/norm(t);

percent_variance_explained = 0;
while (percent_variance_explained < .7) || (K < 2)
    if K >= 20; break; end
    obj = u'*double(ttv(ttv(Xhat,v,2),w,2))*t;
    ind = 1; iter = 0; thr = 1e-6;
    while (ind>thr) && (max_it>iter)
        oldu = u; oldv = v; oldw = w; oldt=t; oldo = obj(end);
  
        uhat = double(ttv(ttv(Xhat,v,2),w,2))*t;
        if norm(uhat)==0
            u = zeros(ns(1),1);
        else
            u = uhat/norm(uhat,2);
        end
        vhat = double(ttv(ttv(Xhat,u,1),w,2))*t;
        if norm(vhat)==0
            v = zeros(ns(2),1);
        else
            v = vhat/norm(vhat,2);
        end
        what = double(ttv(ttv(Xhat,u,1),v,1))*t;
        if norm(what)==0
            w = zeros(ns(3),1);
        else
            w = what/norm(what,2);
        end
        that = double(ttv(ttv(Xhat,u,1),v,1))'*w;
        if norm(that)==0
            t = zeros(ns(4),1);
        else
            %t = that/norm(that,2);
            t = that/norm(that,2);
        end
        
        obj = [obj; u'*double(ttv(ttv(Xhat,v,2),w,2))*t ];
        ind = abs((obj(end) - oldo)/obj(1));
        if verbose; fprintf('Outlier estimation error: %f \n', ind); end
        iter = iter + 1;
    end
   
    d = u'*double(ttv(ttv(Xhat,v,2),w,2))*t;
    Xhat = Xhat - full(ktensor(d,u,v,w,t));
    U = [U u]; V = [V v]; W = [W w]; T = [T t]; D = [D d];    

	Pu = U*((U'*U)\U');
	Pv = V*((V'*V)\V');
	Pw = W*((W'*W)\W');
	Pt = T*((T'*T)\T');
	X_tilde = ttm(ttm(ttm(ttm(X,Pu,1),Pv,2),Pw,3),Pt,4);
	percent_variance_explained = norm(X_tilde)/norm(X); 
    K = K + 1;
end
if verbose; fprintf('There were %d iterations.\n', K); end

projection_tensor =ttm(ttm(ttm(X,V',2),W',3),T',4);
projection_points = zeros(obs,K);
for j = 1:K
    projection_points(:,j) = projection_tensor(:,j,j,j);
end
 
% Create temporary files to save outlier information
uuid = char(java.util.UUID.randomUUID);
tmp_results_dir = fullfile(pwd, strcat('automated_outliers_tmp_',uuid));
if exist(tmp_results_dir, 'dir') == 7; rmdir(tmp_results_dir, 's'); end
mkdir(tmp_results_dir);

% Save points to .mat file
pts_fn = fullfile(tmp_results_dir, 'points.mat');
save(pts_fn, 'projection_points');

% cd to code dir
original_dir = pwd;          % Save original dir
fp = mfilename('fullpath');
[pathstr, ~, ~] = fileparts(fp);
cd(pathstr); addpath(pathstr); savepath;

% Run outliers.R
% system('R CMD BATCH outliers.R outputForDebugging.txt');
% cmd = strcat('R CMD BATCH outliers.R', {' '}, tmp_results_dir, ' outputForDebugging.txt');
% Method that has been prefered more recently:
cmd = strcat('Rscript outliers.R', {' '}, tmp_results_dir);
% fprintf(strcat(cmd{1}, '\n'));
s = system(cmd{1});

% Load 'outliers_input' variable and return list of outliers
clus_fn = fullfile(tmp_results_dir, 'clusterings.mat'); 
load(clus_fn);         
if outliers_input == -1
    outliers = [];
else
    outliers = outliers_input;
end

% Delete temporary directory
if exist(tmp_results_dir, 'dir') == 7; rmdir(tmp_results_dir, 's'); end
cd(original_dir);

end

