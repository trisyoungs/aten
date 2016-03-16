---
title: Scope
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

As with C, variable scope is employed in **Aten** meaning that a variable may be local to certain parts of the code / filter / script. In addition, two or more variables of the same name (but possibly different types) may peacefully co-exist in different scopes within the same code / filter / script. Consider this example:

```
int n = 4, i = 99;
printf("%i\n", i);
if (n == 4)
{
      int i = 0;
      printf("%i\n", i);
}
printf("%i\n", i);
```

...will generate the following output:

```
99
0
99
```

Why? Well, even though two variables called _i_ have legitimately been declared, the second declaration is made within a different block, enclosed within a pair of curly braces. Each time a new block is opened it has access to all of the variables visible within the preceeding scope, but these existing variable names may be re-used within the new block in new declarations, and does **not** affect the original variable. Hence, in the example given above, after the termination of the block the final [printf](/aten/docs/scripting/commands/messaging#printf) statement again sees the original variable _i_, complete with its initial value of 99 intact. Note that if a variable is re-declared within a scoping block, there is no way to access the original variable until the scoping block has been closed. Blocks can be nested to any depth.

One exception to this rule is when user functions are declared – inside the function no variables declared outside the function are visible. Usually this is the safest practice, since it means that the rest of the code cannot be corrupted by a rogue function.  However, on occasion it is useful or necessary to be able to use a function to change a variable which exists in the main program or enclosing scope. Here we introduce the concept of _global_ variables – variables that are declared in either the main program scope or within a function, but are also accessible by any other functions defined within that scope. Such variables can be created by using the global prefix as follows:

```
global int x = 2;
void doubleVariable()
{
      x = 2 * x;
}

# Execute the function
doubleVariable();
printf("x is %i\n", x);
```

This will print "x is 4". Without the  prefix on the declaration of the variable _x_, the code will not compile since, inside the function, the variable _x_ is ‘hidden’ from this scope.

