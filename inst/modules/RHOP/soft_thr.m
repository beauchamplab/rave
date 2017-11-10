function [u] = soft_thr(a, lam, pos)
% SOFT_THR
%
% Soft thresholding function
% 
% INPUT:
%   1. a
%   2. lam
%   3. pos
%
% OUTPUT:
%   1. u
% 
% See sign.m

if pos==0
    % preserve sign
    u = sign(a).*max(abs(a) - lam,0);
else 
    u = max(a - lam,0);
end
end
