function [setupComplete] = rhopls_setup()
% Load required toolboxes and configures Matlab's environment variables
%
%   Ex. setupComplete = rhopls_setup();
%
% OUTPUT:
%   setupComplete: boolean value indictating weather or not the
%   configuration is complete

setupComplete = false;
original_dir = pwd;          % Save original dir

% Find path of all rhopls_code directory
fp = mfilename('fullpath');
[pathstr, ~, ~] = fileparts(fp);
cd(pathstr);
fprintf('%s',pathstr);

% Install YAMLMatlab Toolbox
addpath(fullfile(pathstr,'YAMLMatlab')); savepath; cd(pathstr);

% Read config file
config = ReadYaml('config.yaml');

% Install Tensor Toolbox
cd(config.tensor_toolbox); addpath(pwd); savepath;
cd met; addpath(pwd); savepath;

% Install RHOP Toolbox
cd(pathstr); addpath(pathstr); savepath;

% Configure Matlab environment variable for R
cd(pathstr);
setenv('PATH', [getenv('PATH'), strcat(':', config.r_command)]);
setenv('PATH', [getenv('PATH'), strcat(':', config.rscript_command)]);
setenv('PATH', [getenv('PATH'), strcat(':', config.usr_local_bin)]);
fprintf('Current MATLAB PATH: %s\n', getenv('PATH'))

% Install required R packages
s = system('Rscript setup_R.R');
if s == 0; setupComplete = true; end

cd(original_dir);            % Return to original dir

end
