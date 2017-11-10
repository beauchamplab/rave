function [] = cmd_hopca_cptpa_ecog(fn)
% This function is a command line interface for hopca_cptpa_ecog.m
%
% INPUT 
%   1. fn: The '.h5' file containing parameters
%
% See hopca_cptpa_ecog.m

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
X = h5read(fn, '/ecog/X');
K = h5read(fn, '/params/K');
lamu = h5read(fn, '/params/lamu');
lamv = h5read(fn, '/params/lamv');
lamw = h5read(fn, '/params/lamw');
omega = h5read(fn, '/params/omega');
if omega == 0; omega = []; end
v = logical(h5read(fn, '/params/v'));
maxIterations = h5read(fn, '/params/maxIterations');
%saveOutputAs = h5read(fn, '/params/saveOutputAs');
%saveOutputAs = deblank(strtrim(deblank(saveOutputAs{1})));
saveOutputAs = '';

% Permute X: (trial x freq x tim x elec) --> (trial x elec x freq x tim)
X = permute(X, [1 4 2 3]);

% Run 'hopca_cptpa_ecog.m'
[U, V, W, Ti, D, Xhat, objVals] = hopca_cptpa_ecog(X, K, ...
    'lamu', lamu, 'lamv', lamv, 'lamw', lamw, 'omega', omega, ...
    'verbose', v, 'maxIterations', maxIterations, ...
    'saveOutputAs', saveOutputAs);
Xhat = double(Xhat);

% Permute X:  (trial x elec x freq x tim) --> (trial x freq x tim x elec)
X = permute(X, [1 3 4 2]);

% Save results to hdf5 file
h5create(fn, '/out/Xhat', size(Xhat)); 
h5write(fn, '/out/Xhat', Xhat);
h5create(fn, '/out/U', size(U)); 
h5write(fn, '/out/U', U);
h5create(fn, '/out/V', size(V)); 
h5write(fn, '/out/V', V);
h5create(fn, '/out/W', size(W)); 
h5write(fn, '/out/W', W);
h5create(fn, '/out/Ti', size(Ti)); 
h5write(fn, '/out/Ti', Ti);
h5create(fn, '/out/D', size(D)); 
h5write(fn, '/out/D', D);
h5create(fn, '/out/objVals', size(objVals)); 
h5write(fn, '/out/objVals', objVals);

% Exit matlab program
exit()

end
