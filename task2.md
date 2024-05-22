1. $\Omega = (\lambda x.xx)(\lambda x.xx)$  
  Trying to reduce: $(\lambda x.xx)(\lambda x.xx) \rightarrow (\lambda x.xx)(\lambda x.xx)$
2. $(\lambda x.xxx)(\lambda x.xxx)$  
  Trying to reduce: $(\lambda x.xxx)(\lambda x.xxx) \rightarrow (\lambda x.xxx)(\lambda x.xxx)(\lambda x.xxx)$
3. $Y = \lambda f.(\lambda x.f(xx))(\lambda x.f(xx))$  
  Trying to reduce: $\lambda f.(\lambda x.f(xx))(\lambda x.f(xx)) \rightarrow \lambda f.f((\lambda x.f(xx))(\lambda x.f(xx)))$. Each following step will just keep adding $f$'s.
