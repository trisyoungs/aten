# Known Issues
- Rendering of measurements (esp. angles) is wrong
- Rendering of multiple bonds is inconsistent
- FF -> Manage : List allows multiple selection?
- Ff -> Assign : Doesn't print message?
- Save expression doesn't auto-detect format
- Add SPC-FW ff
- Tiled image saving is broken?
- Secondary surface controls not working 
- Primitive quality preference seems to have no effect when changed from GUI
- No undo/redo on setGeometry calls

## Submitted by S.Turgman (25/02/20)
On Windows 10:
1. Fresh installation of aten2.1.9
2. Open aten and open the pdb file with the following data:
```
HEADER    NONAME 31-Dec-19                                              NONE   1
TITLE                                                                   NONE   2
COMPND    31275                                                         NONE   3
AUTHOR    Apache                                                        NONE   4
REVDAT   1  31-Dec-19     0                                             NONE   5
ATOM      1  O           0      -1.403  -0.000  -0.267  0.00  0.00           O+0
ATOM      2  O           0       1.403   0.000   0.267  0.00  0.00           O+0
ATOM      3  C           0       0.724  -1.148  -0.244  0.00  0.00           C+0
ATOM      4  C           0      -0.724  -1.148   0.244  0.00  0.00           C+0
ATOM      5  C           0       0.724   1.148  -0.244  0.00  0.00           C+0
ATOM      6  C           0      -0.724   1.148   0.244  0.00  0.00           C+0
ATOM      7  H           0       0.740  -1.123  -1.334  0.00  0.00           H+0
ATOM      8  H           0       1.225  -2.051   0.105  0.00  0.00           H+0
ATOM      9  H           0       1.225   2.051   0.105  0.00  0.00           H+0
ATOM     10  H           0       0.740   1.123  -1.334  0.00  0.00           H+0
ATOM     11  H           0      -0.740  -1.123   1.334  0.00  0.00           H+0
ATOM     12  H           0      -1.225  -2.051  -0.105  0.00  0.00           H+0
ATOM     13  H           0      -1.225   2.051  -0.105  0.00  0.00           H+0
ATOM     14  H           0      -0.740   1.123   1.334  0.00  0.00           H+0
CONECT    1    4    6    0    0                                         NONE  20
CONECT    2    3    5    0    0                                         NONE  21
CONECT    3    2    4    7    8                                         NONE  22
CONECT    4    1    3   11   12                                         NONE  23
CONECT    5    2    6    9   10                                         NONE  24
CONECT    6    1    5   13   14                                         NONE  25
END                                                                     NONE  26
```
3. Save the file with the mol2 plugin.
4. aten2 crashes and quits silently.
