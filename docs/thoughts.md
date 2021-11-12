# Thoughts
## 2021/10/14
- Put thoughts on difference between jumps/subroutines and function calls,
  what absraction function calls give and why it's important for optimizations / as optimization boundaries,
  and why need to add labels (or blocks) to abstract this stuff
- Everything starts with core values / ideas of the project. I need to think this through.
  Seems like simplicity / no registers / specialization / focus on high level organization
  (as code caching etc, and symbols abstraction) is more important for me
- Nodes should be reasonably efficient in interpreting but not *that* efficient, they are only intermediate step