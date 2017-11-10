function [ mu_p, mu_q, mu_t ] = get_mean_array_normal(X)
% GET_MEAN_ARRAY_NORMAL     [ mu_p, mu_q, mu_t ] = get_mean_array_normal(X)
%
% This function estimates the array normal mean, assuming that X follows an
% array normal distribution.
%
% INPUT:
%   1. X: Initial data is 4-dimensional, (trials x electrode x freq x
%   time), class 'double' or 'tensor'
%
% OUTPUT:
%   1. mu_p:
%   2. mu_q:
%   3. mu_t:

% Convert X to double
if isa(X, 'tensor'); X = double(X); end

% [n, p, q, t] = size(X);
ns = size(X);
n = ns(1);
p = ns(2);
q = ns(3);
t = ns(4);
max_it = 1000;
tol=1e-6;

M_p = zeros(p,q,t);
M_q = zeros(p,q,t);
M_t = zeros(p,q,t);

mu_p = zeros(1,p);
mu_q = zeros(1,q);
mu_t = zeros(1,t);
X_bar = zeros(p,q,t);
X_bar(:,:,:) = sum(X,1)/n;

for j = 1:max_it    
    mu_p_old = mu_p;
    mu_q_old = mu_q;
    mu_t_old = mu_t;
    
    temp_p = tenmat(X_bar - M_q - M_t,1);
    mu_p = mean(temp_p.data');
    for i = 1:p
        M_p(i,:,:) =  ones(q,t)*mu_p(i);
    end
  
    temp_q = tenmat(X_bar - M_p - M_t,2);
    mu_q = mean(temp_q.data');
    for i = 1:q
        M_q(:,i,:) =  ones(p,t)*mu_q(i);
    end
    
    temp_t = tenmat(X_bar - M_p - M_q,3);
    mu_t = mean(temp_t.data');
    for i = 1:t
        M_t(:,:,i) =  ones(p,q)*mu_t(i);
    end
    
    err = norm(mu_p_old-mu_p) + norm(mu_q_old-mu_q) + norm(mu_t_old-mu_t);
    if err < tol
        break;
    end

end

end

