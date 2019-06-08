C HELP MENU SUBROUTINES
C ---------------------
C WINMENUS
C Help_Start            - How to start, new struct or open a file
C Help_SizLim           - Define problem size first.  Prob size limit
C Help_TitlandSize      - Entering titles and Size
C Help_MatNodSup        - Can enter only matls, node coords, & supports
C Help_Supports         - How supports are modelled
C Help_MemberProps      - Member props needed before  member connections
C Help_ReadyforConnctns - Connections can now be entered
C Help_StrucTables      - Can enter all structure tables
C Help_StrucandLoads    - Can enter all tables incl loads
C Help_LoadTypes        - Available load types
C Help_Output           - Output options
C Help_ReadyToRun       - All data appears to be entered - ready to run
C Help_Baffled          - Contact details for use if stuck.
C
C CALLED SUBROUTINES
C Help_MemberConnctns   - Entering member types, connections, releases
C Help_Supports         - How supports are modelled
C Help_Coords           - Entering node coords and supports
C Help_Loading          - The common format for all load types
C Help_LoadTypes        - Available load types
C Help_LoadCombs        - Entering load combinations



      SUBROUTINE Help_Start
C	  ---------------------
C     GUIDANCE - How to start

      USE dislin
      USE Titles
      CALL MSGBOX('HOW TO START|'//
     +     'Use the FILE menu to OPEN any existing '//
     +     'structure data input file.|'//
     +     'As well as opening your INPUT file, this will try '//
     +     'to create an OUTPUT file with the same filename but '//
     +     'with ''.RTF'' filename extension.  If an .RTF file '//
     +     'of that name already exists, you will be prompted '//
     +     'to rename or overwrite it.||'//

     +     'Alternatively, from the STRUCTURE menu, '//
     +     'select < New structure >, '//
     +     'then choose a file name for the structure.||'//
     +     'This will try to create TWO new files, with the '//
     +     'same filename but different '//
     +     '3-letter filename extensions (the ''file types'').  '//
     +     'One will be a re-usable '//
     +     'data INPUT file, with default file-type, ''.INS'', '//
     +     'for saving all input data.  '//
     +     'The other will be your OUTPUT file, with fixed '//
     +     '''.RTF'' file type, ready '//
     +     'to receive all analysis results.||'//

     +     'Once SAVEd, the input file can be edited either '//
     +     'within the program, or separately, using the system '//
     +     'text editor or any text-reading wordprocessor. The '//
     +     'default input file type (when entering just a '//
     +     'filename without an extension) will be ''.INS''.  '//
     +     'You can over-ride this by appending to the filename '//
     +     'any extension of 1, 2 or 3 letters or numerals '//
     +     '(eg ''.TXT''), as your own file type.||'//

     +     'Your .RTF output results file, with text and '//
     +     'diagrams, will be formatted as a wordprocessor '//
     +     'document, which you can open with (eg) M$Word.')
      RETURN
      END




      SUBROUTINE Help_TitlandSize
C	  ---------------------------
C     GUIDANCE - Titles and structure size

      USE dislin
      USE Titles
      CALL MSGBOX('Use the STRUCTURE menu and select one of:|'//
     +                '  > Titles|'//
     +                '  > Size||'//
     +
     +  'Select ''Titles'' at any time.|'//
     +  'You can then enter up to three lines of titles.||'//
     +
     +  'You need to have selected ''Size'' and entered all the '//
     +  'structure size parameters before entering any numeric data.|'//
     +  'This is so that the program can reserve array storage for '//
     +  'the numbers of materials, member types, nodes, members, |'//
     +  'supports and springs in the structure before it can receive '//
     +  'and store all input data.||'//
     +
     +  'Remember to save your input data using SAVE or SAVE-as.')
      RETURN
      END



      SUBROUTINE Help_SizLim
C	  ----------------------
C     GUIDANCE - Structure size limits

      USE dislin
      USE Titles
      CALL MSGBOX('STRUCTURE SIZE SETTINGS|'//
     + 'Before entering a structure, the problem SIZE '//
     + 'must be defined, using the ''STRUCTURE > Size'' menu.  '//
     + 'This is to reserve memory arrays for the data|'//
     + 'and analysis. To exceed these limits, the structure SIZE|'//
     + 'must first be edited and increased from the above menu.||'//

     + 'Data storage is allocated dynamically, so there are no '//
     + 'individual limits to the numbers of nodes, members, types '//
     + 'of members and materials, rigid or sprung supports, load '//
     + 'cases or load combinations.|'//
     + 'The total of all of the above is limited by the available '//
     + 'memory and virtual memory. Compact ''skyline'' storage of '//
     + 'the stiffness equations enable very large structures '//
     + 'to be analysed.')
      RETURN
      END




      SUBROUTINE Help_MatNodSup
C	  -------------------------
C     GUIDANCE - Materials, Nodes, Supports

      USE dislin
      USE Titles
      CALL MSGBOX('Use the STRUCTURE menu and select|'//
     +            'a data entry table for one of:|'//
     +            '  > Material properties|'//
     +            '  > Node coordinates|'//
     +            '  > Supports.')
      RETURN
      END





      SUBROUTINE Help_MemberProps
C	  ---------------------------
C     GUIDANCE - Member properties needed next

      USE dislin
      USE Titles
      CALL MSGBOX
     +  ('ENTER/EDIT MEMBER PROPERTIES.|'//
     +   'For each member type, the material type and|'//
     +   'section properties Area(X), Area(Y) and Inertia(Z)|'//
     +   'are needed before the members can be entered.|'//
     +   'Use the STRUCTURE menu and select:|'//
     +   '  > Member properties.')
      RETURN
      END





      SUBROUTINE Help_ReadyforConnctns
C	  --------------------------------
C     GUIDANCE - Member connections can now be entered

      USE dislin
      USE Titles
      CALL MSGBOX
     +  ('ENTER/EDIT MEMBER CONNECTIONS.|'//
     +   'Member types & connections are needed for all members.|'//
     +   'Use the STRUCTURE menu and select:|'//
     +   '  > Member connections.')
      RETURN
      END




      SUBROUTINE Help_MemberConnctns
C	  ------------------------------
C     GUIDANCE - Member types, connections, releases

      USE dislin
      USE Titles
      CALL MSGBOX
     +   ('ENTER MEMBER CONNECTIONS, TYPES, & ANY NODE RELEASES.|'//
     + 'Member numbers can be ovewritten.|'//
     + 'Types means the types of member properties .||'//
     + 'Enter any node releases like this:  X   YZ   etc||'//
     + 'To generate members for equal-increment member and '//
     + 'node patterns (all with the same member types, '//
     + 'descriptions and releases), follow this example:|'//
     + '  Member  Descrip   Type     Node1 Releases   Node2 Releases|'//
     + '     9             Beam '//
     + '       4             2                            3        z|'//
     +
     + '   -18.3                   '//
     + '       1             5                            6'//
     + '||(member descriptions & releases are generated as repeats|'//
     + ' of the first line of the sequence, so can be omitted).||'//
     + 'This example would generate:|'//
     + '  Member  Descrip    Type    '//
     + 'Node1 Releases    Node2 Releases|'//
     +
     + '    9              Beam   '//
     + '      4              2                            3        Z|'//
     +
     + '    12            Beam   '//
     + '      3              3                            4        Z|'//
     +
     + '    15            Beam   '//
     + '      2              4                            5        Z|'//
     +
     + '    18            Beam   '//
     + '      1              5                            6        Z|'//
     +
     + '(any values actually entered for member descriptions and '//
     + 'releases will be ignored).||'//
     +
     + 'For guidance on MEMBER-END releases, see the separate '//
     + 'guidance note about them.')

      RETURN
      END




      SUBROUTINE Help_MemberEndReleases
C	  ---------------------------------
C     GUIDANCE - Member-end releases

      USE  dislin
      USE  Titles
      CALL MSGBOX
     +   ('Member-end releases are entered in the member connections '//
     + 'table, as some combination of the letters X, Y and Z'//
     +'(eg Z, XZ, ZY, YXZ, X, XZ) in the RELEASES columns for either '//
     +'end of the member.  Leaving the releases box blank causes the '//
     + 'member-end to be fully fixed into the connection, (although '//
     + 'the connection can still rotate or translate in so far as '//
     + 'member stiffnesses will allow it to do so).  The releases '//
     + 'are in member LOCAL axes.  An X release allows unconstrained '//
     + 'axial movement, Y allows unconstrained shear displacement '//
     + 'Z creates a pinned connection.||'//
     + 'In input data files, as saved by the program and editable '//
     + 'by the program or by a text editor, mMember-end releases are '//
     + 'entered slightly differently from the above. '//
     + 'In input data files, they are specified by an optional '//
     + '6-char. code embedded at the start of the member description '//
     + '(which description goes AT THE END of the line of '//
     + 'data for each member). The 6-char.code shows any required '//
     + 'releases in the x,y,z directions at end 1 then the x,y,z '//
     + 'directions at end 2 for that member||'//
     + 'The coding notation for the member-end releases is:|'//
     + ' '' '',  ''-''  or  ''1''  means NOT RELEASED;|'//
     + ' ''r'',  ''R''  or  ''0''  means RELEASED.||'//
     +
     + 'This release-code is omitted for all members having '//
     + 'no releases at either end.  It is needed whenever any '//
     + 'release is required at either end of a member. ||'//
     : '''  R  R'' or  ''--r-r-'' or ''--R-R-'' (at the start of a '//
     + 'member description) would all put releases on end-1 in the Z-'//
     + 'direction and on end-2 in the Y-direction.|'//
     + '0s and 1s can also be used, signifying fixities, not '//
     + 'releases, so the notation ''110101'' would give the same '//
     + 'releases as in the above example.  In the three line example '//
     + 'below, members 12, 13 & 14 are all described as ''Brace'' '//
     + 'members, of member ''type 3''|.  Member 12 has no releases, '//
     + 'and members 13 & 14 both have Z releases at both ends.|'//
     + '  12, 11, 3, 3,Brace|'//
     + '  13, 12, 5, 3,110110,Brace|'//
     + '  14, 10, 12, 3,--R--R,Brace')

      RETURN
      END



      SUBROUTINE Help_Loading
C	  -----------------------
C     GUIDANCE - Loading

      USE dislin
      USE Titles
      CALL MSGBOX(
     + ' ALL LOAD TYPES HAVE THE SAME LINE FORMAT.|'//
     + ' Each load entry begins with one of these load positions:|'//
     + '     NODE no.|'//
     + '     MEMBER no.|'//
     + '     MEMBER no. with decimal proportion along the member|'//
     + '     MEMBER no. with the letter ''i'' or ''j'' to state'//
     + ' which end of the member|'//
     + '     MEMBER no. with the letter ''m'' to specify the'//
     + ' member mid-point.|'//
     + ' Examples:  15,  15i,  15.33,  15.5,  15m,  15.8,  '//
     + ' 15.999,  15j.||'//
     +
     + ' A load position a certain distance along a member is'//
     + ' expressed as a proportion along it, from its ''i-end'''//
     + ' (lower-numbered) ''start node'', towards its ''j-end'''//
     + ' (higher-numbered) ''end node''.  This is done by entering,'//
     + ' eg, member no. 15.33, to place a load on member 15, at a'//
     + ' position 33% along from its ''i-end'' towards its'//
     + ' ''j-end''.||'//
     +
     + ' For a triangular load (''TDL'') (see Load types), put either'//
     + '  an ''i'' or a ''j'' immediately after the member number, to'//
     + ' specify which end has the maximum load intensity (the apex '//
     + ' the load triangle).  An ''m''positions a TDL''s  apex at '//
     + ' mid-span (an isosceles load triangle), which is the default '//
     + ' so the ''m'' is optional and can be omitted when a TDL is '//
     + ' to be centred.|'//
     + ' TDL load position examples: 15i, 15 or 15m, or 15j.||'//
     +
     + ' Load descriptions can have up to 24 characters.||'//
     +
     + ' Axes are [G]lobal or [L]ocal.||'//
     +
     + ' See also ''Load Types''.')
      RETURN
      END




      SUBROUTINE Help_LoadTypes
C	  -------------------------
C     GUIDANCE - LoadTypes

      USE dislin
      USE Titles
      CALL MSGBOX(
     + ' FIVE LOAD TYPES ARE AVAILABLE:|'//
     + ' N = node load (point load on a node)|'//
     + ' P = point load on a member|'//
     + ' U = UDL on a member|'//
     + ' T = TDL (triangular distributed load) on a member|'//
     + ' D = Self weight of a member (using its Density)|'//
     + ' S = Prescribed displacemt (settlement) of a supported node||'//
     + ' Load values are given in the X, Y, Z direction(s).|'//
     + ' Enter node loads or member point loads as X, Y, Z forces.|'//
     + ' For UDL or TDL, enter load intensity in X, Y, Z directn(s).|'//
     + ' TDL max.load intensity (triangle apex) can be at the member'//
     + ' i-end, j-end or (by default) its mid-point.'//
     + ' Put ''i'', ''j'' or ''m'' immediately after the member no.'//
     + ' to indicate which is required, or omit this for ''m''.|'//
     + ' For self-weight leave X, Y and Z blank to use the material'//
     + ' density in the global-Y direction, or over-ride this by'//
     + ' entering specific densities in the X, Y direction(s).||'//
     +
     + ' For support settlements enter X, Y, Z displacement(s).||'//
     +
     + ' For additional loadcases, select the LC without a number.|'//
     + ' To add more blank lines to a LC, click [More entries].||'//
     +
     + ' Loads below a blank line will not be saved or read. ||'//
     + ' To delete load data, make one of the load data lines blank.|'//
     + ' All the load data lines that come after it will also be'//
     + ' ignored and deleted from that load case.||'//
     +
     + ' See also ''Loading''.')
      RETURN
      END




      SUBROUTINE Help_LoadCombs
C	  -------------------------
C     GUIDANCE - Load Combinations

      USE dislin
      USE Titles
      CALL MSGBOX(
     + ' LOAD COMBINATIONS|'//
     + ' For each load combination: |'//
     + '   - enter the numbers of all load cases you require; |'//
     + '   - leave their load descriptions blank; |'//
     + '   - enter the load factors to appliy to all load cases. ||'//
     +
     + ' For additional combinations, select the one with no number.|'//
     + ' To add more lines to a combination, click [More entries].||'//
     +
     + ' To delete load data from one of the combinations, make one|'//
     + ' of the lines blank.  All the load lines that come after it|'//
     + ' will also be ignored and deleted from that combination.')
      RETURN
      END




      SUBROUTINE Help_Output
C	  ----------------------
C     GUIDANCE - Output

      USE dislin
      USE Titles
      CALL MSGBOX('The OUTPUT menu has options either to '//
     +            'display the structure as a skeletal line '//
     +            'diagram or for line thicknesses to be '//
     +            'scaled to section depths guessed from '//
     +            'the AreaX and InertiaZ values you have '//
     +            'entered.||'//
     +            'You can list just your input data to an '//
     +            'M$Word-readable ''.rtf'' OUTPUT file from '//
     +            'the OUTPUT menu.||'//
     +            'When the structure is fully ready, you '//
     +            'will be able to use the OUTPUT menu to '//
     +            'analyse it. This will automatically save '//
     +            'all results to your M$Word-readable '//
     +            '''.rtf'' OUTPUT file, and you will then '//
     +            'be able to review the results on screen '//
     +            'before deciding whether to keep the '//
     +            '''.rtf'' OUTPUT file or to overwrite it '//
     +            'by editing the data and re-running the '//
     +            'analysis.||'//
     +            'Your .RTF output results file, with text and '//
     +            'diagrams, will be formatted as a wordprocessor '//
     +            'document, which you can open with (eg) M$Word.')
      RETURN
      END




      SUBROUTINE Help_Baffled
C	  -----------------------
C     GUIDANCE - Baffled

      USE dislin
      USE Titles
      CALL MSGBOX('If stuck, please contact John@Wasilewski.co.uk|'//
     +        'or call +44(0)1865 454 170 or +44(0)7900 891 107')
      RETURN
      END




      SUBROUTINE Help_Coords
C     ----------------------
      USE dislin
      USE Titles
      CALL MSGBOX('ENTER/EDIT NODE COORDINATES|'//
     +         'Optionally enter/edit supports and/or springs.||'//
     +         'Node numbers can be ovewritten.||'//
     +         'To generate coordinates for equal-increment|'//
     +         'node patterns, follow this example:|'//
     +         '    Node     X:coord   Y-Coord|'//
     +         '      9           2.0          10.4|'//
     +         '    -18.3       8.0          10.1||'//
     +         'would generate:|'//
     +         '    Node     X:coord   Y-Coord|'//
     +         '    9            2.0           10.4|'//
     +         '    12          4.0           10.3|'//
     +         '    15          6.0           10.2|'//
     +         '    18          8.0           10.1||'//
     +         'Enter rigid supports as directions, eg Z, XY, X+Y+Z.|'//
     +         'Enter springs as stiffnesses.|'//
     +        'Rigid supports and/or springs may be entered here, or|'//
     +         'omitted here and entered in the Structure > Supports|'//
     +         'menu, or entered/edited in both.')
      RETURN
      END




      SUBROUTINE Help_Supports
C     ------------------------
      USE dislin
      USE Titles
      CALL MSGBOX('SUPPORTS|'//
     +            '------------||'//
     +  'Supports have to be entered at the nodes, but as'//
     +  'movement-vector ''restraints,'' not as nodal supports.||'//
     +  'A restraint applies to a single degree of freedom.  '//
     +  'For example, if node 6 is supported in the '//
     +  'X and Y directions, this is considered, not as '//
     +  'ONE SUPPORT, of node 6 in two directions, but '//
     +  'as TWO RESTRAINTS, of node 6, direction X, and node 6, '//
     +  'direction Y.  Two restraints must be counted, in the '//
     +  'number of restraints given when entering the size of the '//
     +  'structure, and two entries are needed in the restraints '//
     +  'table.||'//
     +  'Rigid restraints and springs (sprung restraints) both '//
     +  'count as RESTRAINTS.||'//
     +  'Restraints are entered as a node number followed by,|'//
     +  ' - For rigid restraints:'//
     +  '  ''X'', ''XY'', ''XZ'', ''Y'', ''YZ'', ''Z'' or ''XYZ'';|'//
     +  ' - For sprung restraints:'//
     +  '  Spring stiffnesses in up to three restraint directions.||'//
     +  'When a node will require a prescribed displacement in any '//
     +  'one or more direction(s), forced the required displacement '//
     +  'will be bound to produce a reaction in the structure '//
     +  '(whether as a reaction or as a force trying to increase the '//
     +  'displacement still further).  Prescribed displacements must '//
     +  'therefore be imposed only at restraints.  They are entered '//
     +  'as ''settlement of supports'', as part of the loading input.')
      RETURN
      END



      SUBROUTINE Help_StrucTables
C	  ---------------------------
C     GUIDANCE - Structure tables

      USE dislin
      USE Titles
      CALL MSGBOX('Use the STRUCTURE menu and select|'//
     +                     'a data entry table for one of:|'//
     +                     '   > Material properties|'//
     +                     '   > Member properties|'//
     +                     '   > Node coordinates|'//
     +                     '   > Member positions|'//
     +                     '   > Supports.||'//
     +                     'You can list your input data to an|'//
     +                     'M$Word-readable ''.rtf'' OUTPUT file|'//
     +                     'from the OUTPUT menu.')
      RETURN
      END



      SUBROUTINE Help_StrucandLoads
C     -----------------------------
C     GUIDANCE - Structure tables and loads

      USE dislin
      USE Titles
      CALL MSGBOX('Program '//TRIM(Prog8)//
     +                 ' ver.'//Vers4//'|'//
     +                 REPEAT('-',Len_Trim(Prog8)+
     +                 Len_Trim(Vers4)+15)//'|'//
     +                 'Use the STRUCTURE menu and select a data|'//
     +                 'table for entering or editing any of :|'//
     +                 '   > Material properties|'//
     +                 '   > Member properties|'//
     +                 '   > Node coordinates|'//
     +                 '   > Member positions|'//
     +                 '   > Supports|'//
     +                 '   > Loads.||'//
     +                 'You can list your input data to an|'//
     +                 'M$Word-readable ''.rtf'' OUTPUT file|'//
     +                 'from the OUTPUT menu.')
      RETURN
      END



      SUBROUTINE Help_ReadyToRun
C     ----------------------------
C     GUIDANCE - Structure tables

      USE dislin
      USE Titles
      CALL MSGBOX('Program '//TRIM(Prog8)//
     +                      ' ver.'//Vers4//'|'//
     +                      REPEAT('-',Len_Trim(Prog8)+
     +                      Len_Trim(Vers4)+19)//'|'//
     +        'Select any data entry table from the STRUCTURE|'//
     +        'menu to edit your input data, or you can now use|'//
     +        'the OUTPUT menu to analyse the structure.||'//
     +        'When you run the analysis, this automatically writes|'//
     +        'all input data plus all results to an M$Word-readable|'//
     +        '''.rtf'' output file with the same name as your data|'//
     +        'input file (overwriting that file if it already|'//
     +        'exists).  After running the analysis, you will be|'//
     +        'able to examine the results on-screen before deciding|'//
     +        'whether to keep the ''.rtf'' output file or to over-|'//
     +        'write it by editing some of the data and re-running|'//
     +        'the analysis.||'//
     +        'The OUTPUT menu also lets you write just your input|'//
     +        'data to the ''.rtf'' file without running the|'//
     +        'analysis, and to switch between displaying the|'//
     +        'structure with skeletal lines only or with lines|'//
     +        'scaled to section depths guessed from AreaX and|'//
     +        'InertiaZ values.||'//
     +        'Please note that the program contains error traps|'//
     +        'and two types of equilibrium checks, and it has been|'//
     +        'carefully tested, but the ultimate responsibility|'//
     +        'for verifying the reasonableness and correctness|'//
     +        'of the results is:||'//
     +        '.  .  . ALL YOURS!|||'//
     +        'JMW, |'//PDat9//'.')
      RETURN
      END



      SUBROUTINE Help_Finished
C     ------------------------
C     GUIDANCE - See the results
      USE dislin
      USE Titles
      USE Files
      CALL MSGBOX('Program '//TRIM(Prog8)//' ver.'//Vers4//'|'//
     +     REPEAT('-',Len_Trim(Prog8)+Len_Trim(Vers4)+19)//'|'//
     +     'You should now find the results of your analysis, '//
     +     'formatted and paged as a wordprocessor document, '//
     +     'and saved in file '//
     +     TRIM(F%Path)//TRIM(F%NAME)//'.'//TRIM(F%O%EXT)//'.'//
     +     '||You may like to scroll through the results on '//
     +     'screen before deciding whether to keep the '//
     +     'results file or editing the data, re-running the '//
     +     'analysis and overwriting the file with new '//
     +     'results.||'//
     +     'It is essential to examine all the equilibrium checks '//
     +     'shown in the results and to consider carefully whether '//
     +     'the results seem reasonable in all respects.')
      RETURN
      END



      SUBROUTINE HelpWhatsNew
      USE dislin
      USE Titles
      CALL MSGBOX
     +  ('ADDED/FIXED in version '//VERS4//' ('//PDAT9//') :|'//
     +   '-----------------------------------------|'//
     +   '|'//
     +   'Editing restraints in an existing model''s nodes table is '//
     +   'no longer causing restraints to be miscounted.||'//
     +
     +   'The CONTROL menu reverts to its original name of the '//
     +   '''OUTPUT'' menu, with corresponding corrections to '//
     +   'references to it in the HELP menus.||'//
     +
     +   'After an analysis is complete, editing part of the input '//
     +   'no longer disables other editing menu choices.||'//
     +
     +   'The structure data and results pane is always displayed '//
     +   'all the time.||'//
     +
     +   'Menus are now MUCH more stable.||'//
     +
     +   'Version 2.5 :|'//
     +   '---------------------|'//
     +   '''Info'' menu added.||'//
     +   'Structure diagram shows member-end releases a little '//
     +   'larger and closer to member ends.||'//
     +
     +   'Saving a structure with member-end releases was crashing '//
     +   'the program.|Now fixed.||'//
     +
     +   'Editing the structure after completing an analysis was not '//
     +   'always re-calculating new member stiffnesses.  Now fixed.||'//
     +
     +   'Help menu guidance notes are structured a little better.||'//
     +
     +   'Help menu guidance notes corrected amplified to explain '//
     +   'how member-end releases are coded as saved in and read'//
     +   'from input files.||'//
     +
     +   'When editing NODE coordinates (which is also one way of '//
     +   'editing RESTRAINTS), Selecting ''CANCEL'' is no longer '//
     +   'removing all previously-specified restraints.||'//
     +
     +   'File-opening warning about a previous .rtf file of the '//
     +   'same name altered to a request for a ''Y'' as permission '//
     +   'to overwrite, with a ''N'' instructing renaming the '//
     +   'previous version.||'//
     +
     +   'Code added to try to stop dialog boxes appearing with '//
     +   'tiny fonts.||'//
     +
     +   'References in Help menu guidance notes to the OUTPUT menu '//
     +   'all corrected (it is now called the CONTROL menu.||'//
     +
     +   'Menus again a little more crash-resistant '//
     +   '(but more still to do).||'//
     +
     +   'Previous versions :|'//
     +   '---------------------|'//
     +   '''Info'' menu added.||'//
     +   'Menus now a bit more robust (fewer crashes).||'//
     +   'HELP>Guidance menus improved.||'//
     +   'Input data can now be edited from menus after completing an'//
     +   'analysis and the analysis can then be re-run, without '//
     +   'having to exit/re-start the program first, as '//
     +   'formerly necessary.')
      RETURN
      END



      SUBROUTINE HelpAbout
      USE dislin
      USE Titles
      CALL MSGBOX
     +  ('Program INSTRUCT|'//
     +   '----------------------|'//
     +    DESC48//                       ! Struc.anal.of plane frames
     +   '|by '//AUTH16//FRM32//         ! Jw, civl.enginr, London, Oxfd.

     +   '||Solves bending moments, axial and shear forces, '//
     +   'at member ends of linear elastic|'//

     +   'rigid-joint skeletal frames.  Numbers of nodes, '//
     +   'members, materials, member|'//

     +   'types, springs and loads, are limited only by available '//
     +   'memory and disk storage. |'//

     +   'Compacts stiffness equations by the highly efficient'//
     +   ' ''skyline'' storage method,|'//

     +   'enabling very large problem solutions.||'//
     +   'Version '//VERS4//' : '//PDAT9//'.')  ! Version, date

      RETURN
      END

