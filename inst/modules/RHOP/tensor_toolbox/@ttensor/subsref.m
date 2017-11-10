function a = subsref(t,s)
%SUBSREF Subscripted reference for a ttensor.
%
%   Examples
%   core = tensor(rand(2,2,2));
%   X = TTENSOR(core, rand{4,2), rand(5,2),rand(3,2));
%   X.core %<-- returns core array
%   X.U %<-- returns a cell array of three matrices
%   X.U{1} %<-- returns the matrix corresponding to the first mode.
%
%   See also TTENSOR.
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

switch s(1).type    
    case '.'
        switch s(1).subs
            case {'core','lambda'}
                a = tt_subsubsref(t.core,s);
            case {'U','u'}
                a = tt_subsubsref(t.u,s);
            otherwise
                error(['No such field: ', s.subs]);
        end
    case '()'
	error('Subsref with () not supported for ttensor.');
    case '{}'
	new_s(1).type = '.';
	new_s(1).subs = 'u';
	new_s(2:length(s)+1) = s;
	a = subsref(t, new_s);
     otherwise
        error('Invalid subsref.');
end
