      MODULE MESSGS
C     -------------
C     jw / 06-06-17
C     jw / 06-06-17 last rev


      CHARACTER*10 S10(10)
      CHARACTER*20 S20(10)
      CHARACTER*40 S40(10)
      CHARACTER*60 S60(30)

      DATA S10(1:10)
     +/'          ',
     + '          ',
     + '          ',
     + '          ',
     + '          ',
     + '          ',
     + '          ',
     + '          ',
     + '          ',
     + '          '/

      DATA S20(1:10)
     +/'                    ',
     + '                    ',
     + '                    ',
     + '                    ',
     + '                    ',
     + '                    ',
     + '                    ',
     + '                    ',
     + '                    ',
     + '                    '/

      DATA S40(1:10)
     +/'Structural analysis of plane frames',
     + 'by John Wasilewski, civil engineer.',
     + '                                        ',
     + '                                        ',
     + '                                        ',
     + '                                        ',
     + '                                        ',
     + '                                        ',
     + '                                        ',
     + '                                        '/
       
      DATA S60(1:30)
     +/'Single self-contained program file - nothing to install.    ',
     + 'Solves bending moments, axial & shear forces at member ends ',
     + 'of linear elastic rigid-joint ',
     + 'skeletal frames.  Limits numbers of nodes, ',
     + 'members, materials, member types, springs ',
     + 'and loads only by available memory and disk ',
     + 'storage.  Compacts stiffness equations by',
     + 'the highly efficient ''skyline'' storage method, ',
     + 'enabling very large problem solutions.',
     + 'Use any consistent units for dimensions, forces ',
     + 'and moments.  Use the RH screw rule for',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            ',
     + '                                                            '/
      
      END MODULE MESSGS

