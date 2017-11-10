function V = mttkrp(X,U,n)
%MTTKRP Matricized tensor times Khatri-Rao product for ttensor.
%
%   V = MTTKRP(X,U,n) efficiently calculates the matrix product of the
%   n-mode matricization of X with the Khatri-Rao product of all
%   entries in U, a cell array of matrices, except the nth.  How to
%   most efficiently do this computation depends on the type of tensor
%   involved.
%
%   See also TTENSOR, TTENSOR/TTV
%
%MATLAB Tensor Toolbox.
%Copyright 2015, Sandia Corporation.

% This is the MATLAB Tensor Toolbox by T. Kolda, B. Bader, and others.
% http://www.sandia.gov/~tgkolda/TensorToolbox.
% Copyright (2015) Sandia Corporation. Under the terms of Contract
% DE-AC04-94AL85000, there is a non-exclusive license for use of this
% work by or on behalf of the U.S. Government. Export of this data may
% require a license from the United States Government.
% The full license terms can be found in the file LICENSE.txt

N = ndims(X);

if (n==1)
    R = size(U{2},2);
else
    R = size(U{1},2);
end

% Compute cell array of weights to multiply into core
W = cell(N,1);
for i = [1:n-1,n+1:N]
  W{i} = (X.u{i}' * U{i});
end    
Y = mttkrp(X.core,W,n);

% Find each column of answer by multiplying columns of X.u{n} with weights 
V = X.u{n} * Y;
