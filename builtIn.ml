open Ast

let each =
  {
    Ast.sdesc =
      Ast.SFun
        ( "each",
          Some [ "a"; "b" ],
          ( [
              ("f", Ast.FType ([ Ast.PType ("a", None) ], Ast.PType ("b", None)));
              ("l", Ast.PType ("List", Some [ Ast.PType ("a", None) ]));
            ],
            Ast.PType ("Nothing", None),
            [
              {
                Ast.sdesc =
                  Ast.SBexpr
                    ( {
                        Ast.edesc =
                          Ast.ECases
                            ( Ast.PType ("List", Some [ Ast.PType ("a", None) ]),
                              ( {
                                  Ast.edesc = Ast.EVar "l";
                                  eloc =
                                    {
                                      Ast.pos_fname = "";
                                      pos_lnum = 2;
                                      pos_bol = 60;
                                      pos_cnum = 80;
                                    };
                                  etyp =
                                    Some
                                      (Ast.List
                                         (Ast.Tvar
                                            {
                                              Ast.id = 9;
                                              def =
                                                Some
                                                  (Ast.Tvar
                                                     { Ast.id = 12; def = None });
                                            }));
                                },
                                [] ),
                              [
                                ( "link",
                                  Some [ "x"; "y" ],
                                  [
                                    {
                                      Ast.sdesc =
                                        Ast.SBexpr
                                          ( {
                                              Ast.edesc =
                                                Ast.EBlock
                                                  [
                                                    {
                                                      Ast.sdesc =
                                                        Ast.SBexpr
                                                          ( {
                                                              Ast.edesc =
                                                                Ast.ECall
                                                                  ( "f",
                                                                    [
                                                                      [
                                                                        ( {
                                                                            Ast
                                                                            .edesc =
                                                                              Ast
                                                                              .EVar
                                                                                "x";
                                                                            eloc =
                                                                              {
                                                                                Ast
                                                                                .pos_fname =
                                                                                "";
                                                                                pos_lnum =
                                                                                4;
                                                                                pos_bol =
                                                                                109;
                                                                                pos_cnum =
                                                                                143;
                                                                              };
                                                                            etyp =
                                                                              Some
                                                                                (
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                12;
                                                                                def =
                                                                                None;
                                                                                });
                                                                          },
                                                                          [] );
                                                                      ];
                                                                    ] );
                                                              eloc =
                                                                {
                                                                  Ast.pos_fname =
                                                                    "";
                                                                  pos_lnum = 4;
                                                                  pos_bol = 109;
                                                                  pos_cnum = 144;
                                                                };
                                                              etyp =
                                                                Some
                                                                  (Ast.Tvar
                                                                     {
                                                                       Ast.id =
                                                                         15;
                                                                       def =
                                                                         None;
                                                                     });
                                                            },
                                                            [] );
                                                      sloc =
                                                        {
                                                          Ast.pos_fname = "";
                                                          pos_lnum = 4;
                                                          pos_bol = 109;
                                                          pos_cnum = 144;
                                                        };
                                                      styp =
                                                        Some
                                                          (Ast.Tvar
                                                             {
                                                               Ast.id = 15;
                                                               def = None;
                                                             });
                                                    };
                                                    {
                                                      Ast.sdesc =
                                                        Ast.SBexpr
                                                          ( {
                                                              Ast.edesc =
                                                                Ast.ECall
                                                                  ( "each",
                                                                    [
                                                                      [
                                                                        ( {
                                                                            Ast
                                                                            .edesc =
                                                                              Ast
                                                                              .EVar
                                                                                "f";
                                                                            eloc =
                                                                              {
                                                                                Ast
                                                                                .pos_fname =
                                                                                "";
                                                                                pos_lnum =
                                                                                5;
                                                                                pos_bol =
                                                                                145;
                                                                                pos_cnum =
                                                                                163;
                                                                              };
                                                                            etyp =
                                                                              Some
                                                                                (
                                                                                Ast
                                                                                .Arrow
                                                                                ( 
                                                                                [
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                12;
                                                                                def =
                                                                                None;
                                                                                };
                                                                                ],
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                10;
                                                                                def =
                                                                                None;
                                                                                }
                                                                                ));
                                                                          },
                                                                          [] );
                                                                        ( {
                                                                            Ast
                                                                            .edesc =
                                                                              Ast
                                                                              .EVar
                                                                                "y";
                                                                            eloc =
                                                                              {
                                                                                Ast
                                                                                .pos_fname =
                                                                                "";
                                                                                pos_lnum =
                                                                                5;
                                                                                pos_bol =
                                                                                145;
                                                                                pos_cnum =
                                                                                166;
                                                                              };
                                                                            etyp =
                                                                              Some
                                                                                (
                                                                                Ast
                                                                                .List
                                                                                (
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                12;
                                                                                def =
                                                                                None;
                                                                                }));
                                                                          },
                                                                          [] );
                                                                      ];
                                                                    ] );
                                                              eloc =
                                                                {
                                                                  Ast.pos_fname =
                                                                    "";
                                                                  pos_lnum = 5;
                                                                  pos_bol = 145;
                                                                  pos_cnum = 167;
                                                                };
                                                              etyp =
                                                                Some Ast.Nothing;
                                                            },
                                                            [] );
                                                      sloc =
                                                        {
                                                          Ast.pos_fname = "";
                                                          pos_lnum = 5;
                                                          pos_bol = 145;
                                                          pos_cnum = 167;
                                                        };
                                                      styp = Some Ast.Nothing;
                                                    };
                                                  ];
                                              eloc =
                                                {
                                                  Ast.pos_fname = "";
                                                  pos_lnum = 6;
                                                  pos_bol = 168;
                                                  pos_cnum = 195;
                                                };
                                              etyp = Some Ast.Nothing;
                                            },
                                            [] );
                                      sloc =
                                        {
                                          Ast.pos_fname = "";
                                          pos_lnum = 6;
                                          pos_bol = 168;
                                          pos_cnum = 195;
                                        };
                                      styp = Some Ast.Nothing;
                                    };
                                  ] );
                                ( "empty",
                                  None,
                                  [
                                    {
                                      Ast.sdesc =
                                        Ast.SBexpr
                                          ( {
                                              Ast.edesc = Ast.EVar "nothing";
                                              eloc =
                                                {
                                                  Ast.pos_fname = "";
                                                  pos_lnum = 3;
                                                  pos_bol = 82;
                                                  pos_cnum = 108;
                                                };
                                              etyp = Some Ast.Nothing;
                                            },
                                            [] );
                                      sloc =
                                        {
                                          Ast.pos_fname = "";
                                          pos_lnum = 3;
                                          pos_bol = 82;
                                          pos_cnum = 108;
                                        };
                                      styp = Some Ast.Nothing;
                                    };
                                  ] );
                              ] );
                        eloc =
                          {
                            Ast.pos_fname = "";
                            pos_lnum = 7;
                            pos_bol = 196;
                            pos_cnum = 203;
                          };
                        etyp = Some Ast.Nothing;
                      },
                      [] );
                sloc =
                  {
                    Ast.pos_fname = "";
                    pos_lnum = 7;
                    pos_bol = 196;
                    pos_cnum = 203;
                  };
                styp = Some Ast.Nothing;
              };
            ] ) );
    sloc = { Ast.pos_fname = ""; pos_lnum = 8; pos_bol = 205; pos_cnum = 208 };
    styp =
      Some
        (Ast.Arrow
           ( [
               Ast.Arrow
                 ( [
                     Ast.Tvar
                       {
                         Ast.id = 9;
                         def = Some (Ast.Tvar { Ast.id = 12; def = None });
                       };
                   ],
                   Ast.Tvar { Ast.id = 10; def = None } );
               Ast.List
                 (Ast.Tvar
                    {
                      Ast.id = 9;
                      def = Some (Ast.Tvar { Ast.id = 12; def = None });
                    });
             ],
             Ast.Nothing ));
  }

let fold =
  {
    Ast.sdesc =
      Ast.SFun
        ( "fold",
          Some [ "a"; "b" ],
          ( [
              ( "f",
                Ast.FType
                  ( [ Ast.PType ("a", None); Ast.PType ("b", None) ],
                    Ast.PType ("b", None) ) );
              ("acc", Ast.PType ("b", None));
              ("l", Ast.PType ("List", Some [ Ast.PType ("a", None) ]));
            ],
            Ast.PType ("b", None),
            [
              {
                Ast.sdesc =
                  Ast.SBexpr
                    ( {
                        Ast.edesc =
                          Ast.ECases
                            ( Ast.PType ("List", Some [ Ast.PType ("a", None) ]),
                              ( {
                                  Ast.edesc = Ast.EVar "l";
                                  eloc =
                                    {
                                      Ast.pos_fname = "";
                                      pos_lnum = 11;
                                      pos_bol = 277;
                                      pos_cnum = 296;
                                    };
                                  etyp =
                                    Some
                                      (Ast.List
                                         (Ast.Tvar
                                            {
                                              Ast.id = 19;
                                              def =
                                                Some
                                                  (Ast.Tvar
                                                     { Ast.id = 22; def = None });
                                            }));
                                },
                                [] ),
                              [
                                ( "link",
                                  Some [ "first"; "rest" ],
                                  [
                                    {
                                      Ast.sdesc =
                                        Ast.SBexpr
                                          ( {
                                              Ast.edesc =
                                                Ast.EBlock
                                                  [
                                                    {
                                                      Ast.sdesc =
                                                        Ast.SDecl
                                                          ( false,
                                                            "newAcc",
                                                            None,
                                                            ( {
                                                                Ast.edesc =
                                                                  Ast.ECall
                                                                    ( "f",
                                                                      [
                                                                        [
                                                                          ( {
                                                                              Ast
                                                                              .edesc =
                                                                                Ast
                                                                                .EVar
                                                                                "first";
                                                                              eloc =
                                                                                {
                                                                                Ast
                                                                                .pos_fname =
                                                                                "";
                                                                                pos_lnum =
                                                                                14;
                                                                                pos_bol =
                                                                                351;
                                                                                pos_cnum =
                                                                                373;
                                                                                };
                                                                              etyp =
                                                                                Some
                                                                                (
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                22;
                                                                                def =
                                                                                None;
                                                                                });
                                                                            },
                                                                            []
                                                                          );
                                                                          ( {
                                                                              Ast
                                                                              .edesc =
                                                                                Ast
                                                                                .EVar
                                                                                "acc";
                                                                              eloc =
                                                                                {
                                                                                Ast
                                                                                .pos_fname =
                                                                                "";
                                                                                pos_lnum =
                                                                                14;
                                                                                pos_bol =
                                                                                351;
                                                                                pos_cnum =
                                                                                378;
                                                                                };
                                                                              etyp =
                                                                                Some
                                                                                (
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                20;
                                                                                def =
                                                                                Some
                                                                                (
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                25;
                                                                                def =
                                                                                Some
                                                                                (
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                28;
                                                                                def =
                                                                                None;
                                                                                });
                                                                                });
                                                                                });
                                                                            },
                                                                            []
                                                                          );
                                                                        ];
                                                                      ] );
                                                                eloc =
                                                                  {
                                                                    Ast
                                                                    .pos_fname =
                                                                      "";
                                                                    pos_lnum =
                                                                      14;
                                                                    pos_bol =
                                                                      351;
                                                                    pos_cnum =
                                                                      379;
                                                                  };
                                                                etyp =
                                                                  Some
                                                                    (Ast.Tvar
                                                                       {
                                                                         Ast.id =
                                                                           25;
                                                                         def =
                                                                           Some
                                                                             (Ast
                                                                              .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                28;
                                                                                def =
                                                                                None;
                                                                                });
                                                                       });
                                                              },
                                                              [] ) );
                                                      sloc =
                                                        {
                                                          Ast.pos_fname = "";
                                                          pos_lnum = 14;
                                                          pos_bol = 351;
                                                          pos_cnum = 379;
                                                        };
                                                      styp =
                                                        Some
                                                          (Ast.Tvar
                                                             {
                                                               Ast.id = 25;
                                                               def =
                                                                 Some
                                                                   (Ast.Tvar
                                                                      {
                                                                        Ast.id =
                                                                          28;
                                                                        def =
                                                                          None;
                                                                      });
                                                             });
                                                    };
                                                    {
                                                      Ast.sdesc =
                                                        Ast.SBexpr
                                                          ( {
                                                              Ast.edesc =
                                                                Ast.ECall
                                                                  ( "fold",
                                                                    [
                                                                      [
                                                                        ( {
                                                                            Ast
                                                                            .edesc =
                                                                              Ast
                                                                              .EVar
                                                                                "f";
                                                                            eloc =
                                                                              {
                                                                                Ast
                                                                                .pos_fname =
                                                                                "";
                                                                                pos_lnum =
                                                                                15;
                                                                                pos_bol =
                                                                                380;
                                                                                pos_cnum =
                                                                                396;
                                                                              };
                                                                            etyp =
                                                                              Some
                                                                                (
                                                                                Ast
                                                                                .Arrow
                                                                                ( 
                                                                                [
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                22;
                                                                                def =
                                                                                None;
                                                                                };
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                25;
                                                                                def =
                                                                                Some
                                                                                (
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                28;
                                                                                def =
                                                                                None;
                                                                                });
                                                                                };
                                                                                ],
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                25;
                                                                                def =
                                                                                Some
                                                                                (
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                28;
                                                                                def =
                                                                                None;
                                                                                });
                                                                                }
                                                                                ));
                                                                          },
                                                                          [] );
                                                                        ( {
                                                                            Ast
                                                                            .edesc =
                                                                              Ast
                                                                              .EVar
                                                                                "newAcc";
                                                                            eloc =
                                                                              {
                                                                                Ast
                                                                                .pos_fname =
                                                                                "";
                                                                                pos_lnum =
                                                                                15;
                                                                                pos_bol =
                                                                                380;
                                                                                pos_cnum =
                                                                                404;
                                                                              };
                                                                            etyp =
                                                                              Some
                                                                                (
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                25;
                                                                                def =
                                                                                Some
                                                                                (
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                28;
                                                                                def =
                                                                                None;
                                                                                });
                                                                                });
                                                                          },
                                                                          [] );
                                                                        ( {
                                                                            Ast
                                                                            .edesc =
                                                                              Ast
                                                                              .EVar
                                                                                "rest";
                                                                            eloc =
                                                                              {
                                                                                Ast
                                                                                .pos_fname =
                                                                                "";
                                                                                pos_lnum =
                                                                                15;
                                                                                pos_bol =
                                                                                380;
                                                                                pos_cnum =
                                                                                410;
                                                                              };
                                                                            etyp =
                                                                              Some
                                                                                (
                                                                                Ast
                                                                                .List
                                                                                (
                                                                                Ast
                                                                                .Tvar
                                                                                {
                                                                                Ast
                                                                                .id =
                                                                                22;
                                                                                def =
                                                                                None;
                                                                                }));
                                                                          },
                                                                          [] );
                                                                      ];
                                                                    ] );
                                                              eloc =
                                                                {
                                                                  Ast.pos_fname =
                                                                    "";
                                                                  pos_lnum = 15;
                                                                  pos_bol = 380;
                                                                  pos_cnum = 411;
                                                                };
                                                              etyp =
                                                                Some
                                                                  (Ast.Tvar
                                                                     {
                                                                       Ast.id =
                                                                         28;
                                                                       def =
                                                                         None;
                                                                     });
                                                            },
                                                            [] );
                                                      sloc =
                                                        {
                                                          Ast.pos_fname = "";
                                                          pos_lnum = 15;
                                                          pos_bol = 380;
                                                          pos_cnum = 411;
                                                        };
                                                      styp =
                                                        Some
                                                          (Ast.Tvar
                                                             {
                                                               Ast.id = 28;
                                                               def = None;
                                                             });
                                                    };
                                                  ];
                                              eloc =
                                                {
                                                  Ast.pos_fname = "";
                                                  pos_lnum = 16;
                                                  pos_bol = 412;
                                                  pos_cnum = 421;
                                                };
                                              etyp =
                                                Some
                                                  (Ast.Tvar
                                                     { Ast.id = 28; def = None });
                                            },
                                            [] );
                                      sloc =
                                        {
                                          Ast.pos_fname = "";
                                          pos_lnum = 16;
                                          pos_bol = 412;
                                          pos_cnum = 421;
                                        };
                                      styp =
                                        Some
                                          (Ast.Tvar { Ast.id = 28; def = None });
                                    };
                                  ] );
                                ( "empty",
                                  None,
                                  [
                                    {
                                      Ast.sdesc =
                                        Ast.SBexpr
                                          ( {
                                              Ast.edesc = Ast.EVar "acc";
                                              eloc =
                                                {
                                                  Ast.pos_fname = "";
                                                  pos_lnum = 12;
                                                  pos_bol = 298;
                                                  pos_cnum = 316;
                                                };
                                              etyp =
                                                Some
                                                  (Ast.Tvar
                                                     {
                                                       Ast.id = 20;
                                                       def =
                                                         Some
                                                           (Ast.Tvar
                                                              {
                                                                Ast.id = 25;
                                                                def =
                                                                  Some
                                                                    (Ast.Tvar
                                                                       {
                                                                         Ast.id =
                                                                           28;
                                                                         def =
                                                                           None;
                                                                       });
                                                              });
                                                     });
                                            },
                                            [] );
                                      sloc =
                                        {
                                          Ast.pos_fname = "";
                                          pos_lnum = 12;
                                          pos_bol = 298;
                                          pos_cnum = 316;
                                        };
                                      styp =
                                        Some
                                          (Ast.Tvar
                                             {
                                               Ast.id = 20;
                                               def =
                                                 Some
                                                   (Ast.Tvar
                                                      {
                                                        Ast.id = 25;
                                                        def =
                                                          Some
                                                            (Ast.Tvar
                                                               {
                                                                 Ast.id = 28;
                                                                 def = None;
                                                               });
                                                      });
                                             });
                                    };
                                  ] );
                              ] );
                        eloc =
                          {
                            Ast.pos_fname = "";
                            pos_lnum = 17;
                            pos_bol = 422;
                            pos_cnum = 427;
                          };
                        etyp =
                          Some
                            (Ast.Tvar
                               {
                                 Ast.id = 20;
                                 def =
                                   Some
                                     (Ast.Tvar
                                        {
                                          Ast.id = 25;
                                          def =
                                            Some
                                              (Ast.Tvar
                                                 { Ast.id = 28; def = None });
                                        });
                               });
                      },
                      [] );
                sloc =
                  {
                    Ast.pos_fname = "";
                    pos_lnum = 17;
                    pos_bol = 422;
                    pos_cnum = 427;
                  };
                styp =
                  Some
                    (Ast.Tvar
                       {
                         Ast.id = 20;
                         def =
                           Some
                             (Ast.Tvar
                                {
                                  Ast.id = 25;
                                  def =
                                    Some (Ast.Tvar { Ast.id = 28; def = None });
                                });
                       });
              };
            ] ) );
    sloc = { Ast.pos_fname = ""; pos_lnum = 18; pos_bol = 428; pos_cnum = 431 };
    styp =
      Some
        (Ast.Arrow
           ( [
               Ast.Arrow
                 ( [
                     Ast.Tvar
                       {
                         Ast.id = 19;
                         def = Some (Ast.Tvar { Ast.id = 22; def = None });
                       };
                     Ast.Tvar
                       {
                         Ast.id = 20;
                         def =
                           Some
                             (Ast.Tvar
                                {
                                  Ast.id = 25;
                                  def =
                                    Some (Ast.Tvar { Ast.id = 28; def = None });
                                });
                       };
                   ],
                   Ast.Tvar
                     {
                       Ast.id = 20;
                       def =
                         Some
                           (Ast.Tvar
                              {
                                Ast.id = 25;
                                def =
                                  Some (Ast.Tvar { Ast.id = 28; def = None });
                              });
                     } );
               Ast.Tvar
                 {
                   Ast.id = 20;
                   def =
                     Some
                       (Ast.Tvar
                          {
                            Ast.id = 25;
                            def = Some (Ast.Tvar { Ast.id = 28; def = None });
                          });
                 };
               Ast.List
                 (Ast.Tvar
                    {
                      Ast.id = 19;
                      def = Some (Ast.Tvar { Ast.id = 22; def = None });
                    });
             ],
             Ast.Tvar
               {
                 Ast.id = 20;
                 def =
                   Some
                     (Ast.Tvar
                        {
                          Ast.id = 25;
                          def = Some (Ast.Tvar { Ast.id = 28; def = None });
                        });
               } ));
  }
