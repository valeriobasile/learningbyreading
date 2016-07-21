% This file was automatically generated on Tue Sep 22 12:37:48 CEST 2015
% by gsb/src/scripts/pronouns-boxer.sh
% ana:[n:female,n:male,n:thing]
% ant:[sort,symb,type]
:- module(antecedent,[ana_ant_sort/3,ana_ant_symb/3,
                      sentence_position_ant/2,same_sentence/4,pos_ant/2,extractFeaturesAna/2,extractFeaturesAnt/2]).
ana_ant_sort(n:female,per,0.31305605403777187):- !. % 4719/9703=0.4863444295578687
ana_ant_sort(n:female,n,0.41322233828692323):- !. % 3747/9703=0.3861692260125734
ana_ant_sort(n:female,org,1.3038589931418716):- !. % 482/9703=0.049675358136658763
ana_ant_sort(n:female,geo,1.4428379870304455):- !. % 350/9703=0.036071318149026074
ana_ant_sort(n:female,nam,1.5156143203217827):- !. % 296/9703=0.030506029063176336
ana_ant_sort(n:female,a,2.1945143418824675):- !. % 62/9703=0.006389776357827476
ana_ant_sort(n:female,tim,2.5555422672217336):- !. % 27/9703=0.0027826445429248686
ana_ant_sort(n:female,_,2.68587603571674):- !. % add-20 smoothing 
ana_ant_sort(n:male,per,0.29142289671043453):- !. % 10467/20476=0.5111838249658136
ana_ant_sort(n:male,n,0.4207123289504629):- !. % 7772/20476=0.3795663215471772
ana_ant_sort(n:male,org,1.3502989251443764):- !. % 914/20476=0.0446376245360422
ana_ant_sort(n:male,nam,1.4367633031787412):- !. % 749/20476=0.036579410041023636
ana_ant_sort(n:male,geo,1.7360572759505468):- !. % 376/20476=0.018362961515921077
ana_ant_sort(n:male,a,2.210874575760645):- !. % 126/20476=0.006153545614377808
ana_ant_sort(n:male,tim,2.5952417772434084):- !. % 52/20476=0.0025395585075210004
ana_ant_sort(n:male,_,3.0102151252142266):- !. % add-20 smoothing 
ana_ant_sort(_,_,10):- !. % all other cases 
ana_ant_symb(n:female,female,0.4280589744612249):- !. % 2565/6873=0.3731994762112615
ana_ant_symb(n:female,'ms.',1.0640916505447973):- !. % 593/6873=0.08627964498763277
ana_ant_symb(n:female,rice,1.082798008198041):- !. % 568/6873=0.08264222319220137
ana_ant_symb(n:female,condoleezza,1.4907933694584212):- !. % 222/6873=0.032300305543430816
ana_ant_symb(n:female,clinton,1.5128638886113672):- !. % 211/6873=0.030699839953441
ana_ant_symb(n:female,secretary,1.5149270491751408):- !. % 210/6873=0.030554343081623744
ana_ant_symb(n:female,woman,1.5561129766613324):- !. % 191/6873=0.027789902517095882
ana_ant_symb(n:female,'u.s.',1.6092596392953864):- !. % 169/6873=0.024588971337116253
ana_ant_symb(n:female,merkel,1.8077625662238503):- !. % 107/6873=0.015568165284446385
ana_ant_symb(n:female,tymoshenko,1.8371463439090598):- !. % 100/6873=0.014549687181725593
ana_ant_symb(n:female,'aung~san~suu~kyi',1.8548751108694916):- !. % 96/6873=0.013967699694456569
ana_ant_symb(n:female,'u.s.~secretary~of~state',1.9128670578471783):- !. % 84/6873=0.012221737232649499
ana_ant_symb(n:female,hingis,1.9738234837886042):- !. % 73/6873=0.010621271642659682
ana_ant_symb(n:female,hansen,1.9798138474777915):- !. % 72/6873=0.010475774770842427
ana_ant_symb(n:female,doe,2.0176024083671913):- !. % 66/6873=0.00960279353993889
ana_ant_symb(n:female,wife,2.030966369925173):- !. % 64/6873=0.00931179979630438
ana_ant_symb(n:female,hillary,2.051816508898293):- !. % 61/6873=0.008875309180852612
ana_ant_symb(n:female,spokeswoman,2.066294332266916):- !. % 59/6873=0.0085843154372181
ana_ant_symb(n:female,'mrs.',2.066294332266916):- !. % 59/6873=0.0085843154372181
ana_ant_symb(n:female,girl,2.112870474308271):- !. % 53/6873=0.007711334206314565
ana_ant_symb(n:female,topic,2.1381763395730413):- !. % 50/6873=0.007274843590862797
ana_ant_symb(n:female,pinochet,2.155905106533473):- !. % 48/6873=0.006983849847228284
ana_ant_symb(n:female,bachelet,2.1650484859733425):- !. % 47/6873=0.006838352975411029
ana_ant_symb(n:female,berenson,2.1743885122274857):- !. % 46/6873=0.006692856103593773
ana_ant_symb(n:female,angela,2.183933830133716):- !. % 45/6873=0.006547359231776516
ana_ant_symb(n:female,mowlam,2.2243624871893246):- !. % 41/6873=0.005965371744507493
ana_ant_symb(n:female,england,2.2243624871893246):- !. % 41/6873=0.005965371744507493
ana_ant_symb(n:female,krajicek,2.2350863525810976):- !. % 40/6873=0.005819874872690237
ana_ant_symb(n:female,cat,2.2460817368825605):- !. % 39/6873=0.005674378000872981
ana_ant_symb(n:female,witty,2.25736274729225):- !. % 38/6873=0.0055288811290557256
ana_ant_symb(n:female,thing,2.25736274729225):- !. % 38/6873=0.0055288811290557256
ana_ant_symb(n:female,hassan,2.268944619842065):- !. % 37/6873=0.005383384257238469
ana_ant_symb(n:female,sister,2.280843843141773):- !. % 36/6873=0.005237887385421213
ana_ant_symb(n:female,lohan,2.280843843141773):- !. % 36/6873=0.005237887385421213
ana_ant_symb(n:female,daughter,2.280843843141773):- !. % 36/6873=0.005237887385421213
ana_ant_symb(n:female,person,2.2930782995587844):- !. % 35/6873=0.0050923905136039576
ana_ant_symb(n:female,cho,2.2930782995587844):- !. % 35/6873=0.0050923905136039576
ana_ant_symb(n:female,hughes,2.3056674268668047):- !. % 34/6873=0.004946893641786702
ana_ant_symb(n:female,bhutto,2.3056674268668047):- !. % 34/6873=0.004946893641786702
ana_ant_symb(n:female,crow,2.3186324040311725):- !. % 33/6873=0.004801396769969445
ana_ant_symb(n:female,owl,2.331996365589154):- !. % 32/6873=0.00465589989815219
ana_ant_symb(n:female,'jill~carroll',2.331996365589154):- !. % 32/6873=0.00465589989815219
ana_ant_symb(n:female,'prime~minister',2.3600250891893975):- !. % 30/6873=0.004364906154517678
ana_ant_symb(n:female,president,2.3600250891893975):- !. % 30/6873=0.004364906154517678
ana_ant_symb(n:female,chancellor,2.3600250891893975):- !. % 30/6873=0.004364906154517678
ana_ant_symb(n:female,journalist,2.374748346010104):- !. % 29/6873=0.004219409282700422
ana_ant_symb(n:female,madonna,2.3899883125668406):- !. % 28/6873=0.004073912410883166
ana_ant_symb(n:female,'u.s.~secretary~of~state~hillary',2.422172995938242):- !. % 26/6873=0.003782918667248654
ana_ant_symb(n:female,carroll,2.439206335237022):- !. % 25/6873=0.0036374217954313983
ana_ant_symb(n:female,mother,2.456935102197454):- !. % 24/6873=0.003491924923614142
ana_ant_symb(n:female,saberi,2.475418507891467):- !. % 23/6873=0.0033464280517968865
ana_ant_symb(n:female,kostelic,2.475418507891467):- !. % 23/6873=0.0033464280517968865
ana_ant_symb(n:female,jolie,2.475418507891467):- !. % 23/6873=0.0033464280517968865
ana_ant_symb(n:female,augusto,2.494723663086854):- !. % 22/6873=0.0032009311799796303
ana_ant_symb(n:female,ashia,2.494723663086854):- !. % 22/6873=0.0032009311799796303
ana_ant_symb(n:female,lynndie,2.5149270491751405):- !. % 21/6873=0.0030554343081623746
ana_ant_symb(n:female,fox,2.5149270491751405):- !. % 21/6873=0.0030554343081623746
ana_ant_symb(n:female,_,2.536116348245079):- !. % add-20 smoothing 
ana_ant_symb(n:male,male,0.5152453246890917):- !. % 4385/14362=0.30531959337139675
ana_ant_symb(n:male,'mr.',1.0145126766535353):- !. % 1389/14362=0.09671354964489626
ana_ant_symb(n:male,president,1.0163927422818404):- !. % 1383/14362=0.09629578053195934
ana_ant_symb(n:male,bush,1.44540769334996):- !. % 515/14362=0.03585851552708536
ana_ant_symb(n:male,man,1.6270152241880689):- !. % 339/14362=0.023603954880935803
ana_ant_symb(n:male,'prime~minister',1.7716086487928389):- !. % 243/14362=0.016919649073945133
ana_ant_symb(n:male,chavez,1.7770036806795448):- !. % 240/14362=0.016710764517476676
ana_ant_symb(n:male,'u.s.',1.989897587642975):- !. % 147/14362=0.010235343266954463
ana_ant_symb(n:male,minister,1.9988524302959012):- !. % 144/14362=0.010026458710486005
ana_ant_symb(n:male,ass,2.007995809735771):- !. % 141/14362=0.009817574154017546
ana_ant_symb(n:male,hugo,2.014200122137056):- !. % 139/14362=0.009678317783038574
ana_ant_symb(n:male,thing,2.0432715700843143):- !. % 130/14362=0.0090516641136332
ana_ant_symb(n:male,fox,2.046625212091902):- !. % 129/14362=0.008982035928143712
ana_ant_symb(n:male,castro,2.078033676343526):- !. % 120/14362=0.008355382258738338
ana_ant_symb(n:male,abbas,2.0853329150850257):- !. % 118/14362=0.008216125887759365
ana_ant_symb(n:male,wolf,2.0890290606449895):- !. % 117/14362=0.008146497702269878
ana_ant_symb(n:male,lion,2.0927569331642326):- !. % 116/14362=0.008076869516780393
ana_ant_symb(n:male,general,2.131909057126381):- !. % 106/14362=0.007380587661885531
ana_ant_symb(n:male,obama,2.136025623321213):- !. % 105/14362=0.007310959476396045
ana_ant_symb(n:male,mubarak,2.1528935486085086):- !. % 101/14362=0.007032446734438101
ana_ant_symb(n:male,mahmoud,2.1528935486085086):- !. % 101/14362=0.007032446734438101
ana_ant_symb(n:male,john,2.161579727793601):- !. % 99/14362=0.006893190363459129
ana_ant_symb(n:male,sharon,2.1840870687914524):- !. % 94/14362=0.006545049436011698
ana_ant_symb(n:male,pope,2.1981735300700573):- !. % 91/14362=0.0063361648795432395
ana_ant_symb(n:male,person,2.238136830015077):- !. % 83/14362=0.00577913939562735
ana_ant_symb(n:male,karzai,2.238136830015077):- !. % 83/14362=0.00577913939562735
ana_ant_symb(n:male,yushchenko,2.2541249353992074):- !. % 80/14362=0.005570254839158892
ana_ant_symb(n:male,milosevic,2.3059565736720757):- !. % 71/14362=0.0049436011697535165
ana_ant_symb(n:male,leader,2.3121168823768943):- !. % 70/14362=0.00487397298426403
ana_ant_symb(n:male,cheney,2.3121168823768943):- !. % 70/14362=0.00487397298426403
ana_ant_symb(n:male,boy,2.3121168823768943):- !. % 70/14362=0.00487397298426403
ana_ant_symb(n:male,spokesman,2.371885087380384):- !. % 61/14362=0.004247319314858655
ana_ant_symb(n:male,hamid,2.371885087380384):- !. % 61/14362=0.004247319314858655
ana_ant_symb(n:male,singh,2.393786928828214):- !. % 58/14362=0.004038434758390197
ana_ant_symb(n:male,topic,2.4013400667186597):- !. % 57/14362=0.0039688065729007104
ana_ant_symb(n:male,pinochet,2.4013400667186597):- !. % 57/14362=0.0039688065729007104
ana_ant_symb(n:male,ariel,2.4090268953849505):- !. % 56/14362=0.003899178387411224
ana_ant_symb(n:male,musharraf,2.416852232896907):- !. % 55/14362=0.0038295502019217377
ana_ant_symb(n:male,barack,2.4248211625681826):- !. % 54/14362=0.0037599220164322516
ana_ant_symb(n:male,ahmadinejad,2.4496447462932145):- !. % 51/14362=0.0035510374599637936
ana_ant_symb(n:male,senator,2.4670188423626374):- !. % 49/14362=0.0034117810889848213
ana_ant_symb(n:male,blair,2.4670188423626374):- !. % 49/14362=0.0034117810889848213
ana_ant_symb(n:male,hosni,2.4759736850155636):- !. % 48/14362=0.003342152903495335
ana_ant_symb(n:male,hariri,2.4759736850155636):- !. % 48/14362=0.003342152903495335
ana_ant_symb(n:male,fidel,2.4851170644554337):- !. % 47/14362=0.003272524718005849
ana_ant_symb(n:male,preval,2.494457090709577):- !. % 46/14362=0.003202896532516363
ana_ant_symb(n:male,lawyer,2.5040024086158073):- !. % 45/14362=0.0031332683470268767
ana_ant_symb(n:male,katsav,2.5040024086158073):- !. % 45/14362=0.0031332683470268767
ana_ant_symb(n:male,eagle,2.5137622459049633):- !. % 44/14362=0.0030636401615373905
ana_ant_symb(n:male,viktor,2.5237464668115646):- !. % 43/14362=0.0029940119760479044
ana_ant_symb(n:male,bomber,2.5237464668115646):- !. % 43/14362=0.0029940119760479044
ana_ant_symb(n:male,wilson,2.5339656319932504):- !. % 42/14362=0.0029243837905584182
ana_ant_symb(n:male,saddam,2.5339656319932504):- !. % 42/14362=0.0029243837905584182
ana_ant_symb(n:male,morales,2.5444310656714153):- !. % 41/14362=0.002854755605068932
ana_ant_symb(n:male,annan,2.5444310656714153):- !. % 41/14362=0.002854755605068932
ana_ant_symb(n:male,crane,2.5551549310631887):- !. % 40/14362=0.002785127419579446
ana_ant_symb(n:male,putin,2.5774313257743406):- !. % 38/14362=0.0026458710486004736
ana_ant_symb(n:male,pontiff,2.5774313257743406):- !. % 38/14362=0.0026458710486004736
ana_ant_symb(n:male,fisherman,2.5774313257743406):- !. % 38/14362=0.0026458710486004736
ana_ant_symb(n:male,dog,2.5774313257743406):- !. % 38/14362=0.0026458710486004736
ana_ant_symb(n:male,khodorkovsky,2.5890131983241558):- !. % 37/14362=0.0025762428631109875
ana_ant_symb(n:male,saakashvili,2.6009124216238635):- !. % 36/14362=0.0025066146776215013
ana_ant_symb(n:male,official,2.6009124216238635):- !. % 36/14362=0.0025066146776215013
ana_ant_symb(n:male,foreign,2.6009124216238635):- !. % 36/14362=0.0025066146776215013
ana_ant_symb(n:male,tony,2.6131468780408755):- !. % 35/14362=0.002436986492132015
ana_ant_symb(n:male,howard,2.625736005348896):- !. % 34/14362=0.002367358306642529
ana_ant_symb(n:male,horse,2.625736005348896):- !. % 34/14362=0.002367358306642529
ana_ant_symb(n:male,vladimir,2.6387009825132637):- !. % 33/14362=0.002297730121153043
ana_ant_symb(n:male,very,2.6387009825132637):- !. % 33/14362=0.002297730121153043
ana_ant_symb(n:male,pervez,2.6387009825132637):- !. % 33/14362=0.002297730121153043
ana_ant_symb(n:male,hu,2.6387009825132637):- !. % 33/14362=0.002297730121153043
ana_ant_symb(n:male,george,2.6387009825132637):- !. % 33/14362=0.002297730121153043
ana_ant_symb(n:male,chirac,2.6387009825132637):- !. % 33/14362=0.002297730121153043
ana_ant_symb(n:male,spector,2.652064944071245):- !. % 32/14362=0.0022281019356635567
ana_ant_symb(n:male,serpent,2.652064944071245):- !. % 32/14362=0.0022281019356635567
ana_ant_symb(n:male,schroeder,2.652064944071245):- !. % 32/14362=0.0022281019356635567
ana_ant_symb(n:male,mikhail,2.652064944071245):- !. % 32/14362=0.0022281019356635567
ana_ant_symb(n:male,jackson,2.652064944071245):- !. % 32/14362=0.0022281019356635567
ana_ant_symb(n:male,female,2.652064944071245):- !. % 32/14362=0.0022281019356635567
ana_ant_symb(n:male,aristide,2.652064944071245):- !. % 32/14362=0.0022281019356635567
ana_ant_symb(n:male,rumsfeld,2.6658532285568786):- !. % 31/14362=0.0021584737501740706
ana_ant_symb(n:male,johnston,2.6658532285568786):- !. % 31/14362=0.0021584737501740706
ana_ant_symb(n:male,buckles,2.6658532285568786):- !. % 31/14362=0.0021584737501740706
ana_ant_symb(n:male,raven,2.6800936676714886):- !. % 30/14362=0.0020888455646845844
ana_ant_symb(n:male,netanyahu,2.6800936676714886):- !. % 30/14362=0.0020888455646845844
ana_ant_symb(n:male,judge,2.6800936676714886):- !. % 30/14362=0.0020888455646845844
ana_ant_symb(n:male,delp,2.6800936676714886):- !. % 30/14362=0.0020888455646845844
ana_ant_symb(n:male,aziz,2.6800936676714886):- !. % 30/14362=0.0020888455646845844
ana_ant_symb(n:male,'little~johnny',2.694816924492195):- !. % 29/14362=0.0020192173791950983
ana_ant_symb(n:male,jay,2.694816924492195):- !. % 29/14362=0.0020192173791950983
ana_ant_symb(n:male,holmes,2.694816924492195):- !. % 29/14362=0.0020192173791950983
ana_ant_symb(n:male,doctor,2.694816924492195):- !. % 29/14362=0.0020192173791950983
ana_ant_symb(n:male,shepherd,2.7100568910489318):- !. % 28/14362=0.001949589193705612
ana_ant_symb(n:male,kofi,2.7100568910489318):- !. % 28/14362=0.001949589193705612
ana_ant_symb(n:male,'john~paul',2.7100568910489318):- !. % 28/14362=0.001949589193705612
ana_ant_symb(n:male,head,2.7100568910489318):- !. % 28/14362=0.001949589193705612
ana_ant_symb(n:male,'foreign~minister',2.7100568910489318):- !. % 28/14362=0.001949589193705612
ana_ant_symb(n:male,elton,2.7100568910489318):- !. % 28/14362=0.001949589193705612
ana_ant_symb(n:male,chambers,2.7100568910489318):- !. % 28/14362=0.001949589193705612
ana_ant_symb(n:male,bat,2.7100568910489318):- !. % 28/14362=0.001949589193705612
ana_ant_symb(n:male,augusto,2.7100568910489318):- !. % 28/14362=0.001949589193705612
ana_ant_symb(n:male,olmert,2.725851158232164):- !. % 27/14362=0.0018799610082161258
ana_ant_symb(n:male,cat,2.725851158232164):- !. % 27/14362=0.0018799610082161258
ana_ant_symb(n:male,'secretary-general',2.742241574420333):- !. % 26/14362=0.0018103328227266396
ana_ant_symb(n:male,member,2.742241574420333):- !. % 26/14362=0.0018103328227266396
ana_ant_symb(n:male,khan,2.742241574420333):- !. % 26/14362=0.0018103328227266396
ana_ant_symb(n:male,hawk,2.742241574420333):- !. % 26/14362=0.0018103328227266396
ana_ant_symb(n:male,gotovina,2.742241574420333):- !. % 26/14362=0.0018103328227266396
ana_ant_symb(n:male,dictator,2.742241574420333):- !. % 26/14362=0.0018103328227266396
ana_ant_symb(n:male,secretary,2.7592749137191133):- !. % 25/14362=0.0017407046372371537
ana_ant_symb(n:male,natwar,2.7592749137191133):- !. % 25/14362=0.0017407046372371537
ana_ant_symb(n:male,rudolph,2.777003680679545):- !. % 24/14362=0.0016710764517476676
ana_ant_symb(n:male,mohammed,2.777003680679545):- !. % 24/14362=0.0016710764517476676
ana_ant_symb(n:male,gamal,2.777003680679545):- !. % 24/14362=0.0016710764517476676
ana_ant_symb(n:male,berlusconi,2.777003680679545):- !. % 24/14362=0.0016710764517476676
ana_ant_symb(n:male,jintao,2.795487086373558):- !. % 23/14362=0.0016014482662581814
ana_ant_symb(n:male,ganji,2.795487086373558):- !. % 23/14362=0.0016014482662581814
ana_ant_symb(n:male,editor,2.795487086373558):- !. % 23/14362=0.0016014482662581814
ana_ant_symb(n:male,benedict,2.795487086373558):- !. % 23/14362=0.0016014482662581814
ana_ant_symb(n:male,williams,2.8147922415689446):- !. % 22/14362=0.0015318200807686953
ana_ant_symb(n:male,trial,2.8147922415689446):- !. % 22/14362=0.0015318200807686953
ana_ant_symb(n:male,rene,2.8147922415689446):- !. % 22/14362=0.0015318200807686953
ana_ant_symb(n:male,petraeus,2.8147922415689446):- !. % 22/14362=0.0015318200807686953
ana_ant_symb(n:male,michael,2.8147922415689446):- !. % 22/14362=0.0015318200807686953
ana_ant_symb(n:male,hussein,2.8147922415689446):- !. % 22/14362=0.0015318200807686953
ana_ant_symb(n:male,home,2.8147922415689446):- !. % 22/14362=0.0015318200807686953
ana_ant_symb(n:male,goatherd,2.8147922415689446):- !. % 22/14362=0.0015318200807686953
ana_ant_symb(n:male,ban,2.8147922415689446):- !. % 22/14362=0.0015318200807686953
ana_ant_symb(n:male,mladic,2.8349956276572317):- !. % 21/14362=0.0014621918952792091
ana_ant_symb(n:male,_,2.85618492672717):- !. % add-20 smoothing 
ana_ant_symb(n:X,X,0):- !. % all other cases 
ana_ant_symb(_, _,10):- !. % all other cases 
same_sentence(n:male,  1, 0, 10.00):- !. % (0/963)
same_sentence(n:female,1, 0, 10.00):- !. % (0/963)
same_sentence(n:thing, 1, 0, 10.00):- !. % (0/963)
same_sentence(n:male,  2, 0, 10.00):- !. % (0/130)
same_sentence(n:female,2, 0, 10.00):- !. % (0/130)
same_sentence(n:thing, 2, 0, 10.00):- !. % (0/130)
same_sentence(n:male,  3, 0, 0.75):- !. % (32/180)
same_sentence(n:female,3, 0, 0.75):- !. % (32/180)
same_sentence(n:thing, 3, 0, 0.75):- !. % (32/180)
same_sentence(n:male,  4, 0, 0.36):- !. % (96/220)
same_sentence(n:female,4, 0, 0.36):- !. % (96/220)
same_sentence(n:thing, 4, 0, 0.36):- !. % (96/220)
same_sentence(n:male,  5, 0, 0.29):- !. % (91/178)
same_sentence(n:female,5, 0, 0.29):- !. % (91/178)
same_sentence(n:thing, 5, 0, 0.29):- !. % (91/178)
same_sentence(n:male,  6, 0, 0.20):- !. % (139/220)
same_sentence(n:female,6, 0, 0.20):- !. % (139/220)
same_sentence(n:thing, 6, 0, 0.20):- !. % (139/220)
same_sentence(n:male,  7, 0, 0.22):- !. % (118/195)
same_sentence(n:female,7, 0, 0.22):- !. % (118/195)
same_sentence(n:thing, 7, 0, 0.22):- !. % (118/195)
same_sentence(n:male,  8, 0, 0.19):- !. % (128/198)
same_sentence(n:female,8, 0, 0.19):- !. % (128/198)
same_sentence(n:thing, 8, 0, 0.19):- !. % (128/198)
same_sentence(n:male,  9, 0, 0.14):- !. % (127/176)
same_sentence(n:female,9, 0, 0.14):- !. % (127/176)
same_sentence(n:thing, 9, 0, 0.14):- !. % (127/176)
same_sentence(n:male,  10, 0, 0.17):- !. % (138/203)
same_sentence(n:female,10, 0, 0.17):- !. % (138/203)
same_sentence(n:thing, 10, 0, 0.17):- !. % (138/203)
same_sentence(n:male,  11, 0, 0.13):- !. % (141/190)
same_sentence(n:female,11, 0, 0.13):- !. % (141/190)
same_sentence(n:thing, 11, 0, 0.13):- !. % (141/190)
same_sentence(n:male,  12, 0, 0.11):- !. % (146/189)
same_sentence(n:female,12, 0, 0.11):- !. % (146/189)
same_sentence(n:thing, 12, 0, 0.11):- !. % (146/189)
same_sentence(n:male,  13, 0, 0.12):- !. % (131/173)
same_sentence(n:female,13, 0, 0.12):- !. % (131/173)
same_sentence(n:thing, 13, 0, 0.12):- !. % (131/173)
same_sentence(n:male,  14, 0, 0.10):- !. % (134/168)
same_sentence(n:female,14, 0, 0.10):- !. % (134/168)
same_sentence(n:thing, 14, 0, 0.10):- !. % (134/168)
same_sentence(n:male,  15, 0, 0.10):- !. % (154/193)
same_sentence(n:female,15, 0, 0.10):- !. % (154/193)
same_sentence(n:thing, 15, 0, 0.10):- !. % (154/193)
same_sentence(n:male,  16, 0, 0.08):- !. % (139/167)
same_sentence(n:female,16, 0, 0.08):- !. % (139/167)
same_sentence(n:thing, 16, 0, 0.08):- !. % (139/167)
same_sentence(n:male,  17, 0, 0.06):- !. % (140/162)
same_sentence(n:female,17, 0, 0.06):- !. % (140/162)
same_sentence(n:thing, 17, 0, 0.06):- !. % (140/162)
same_sentence(n:male,  18, 0, 0.06):- !. % (141/161)
same_sentence(n:female,18, 0, 0.06):- !. % (141/161)
same_sentence(n:thing, 18, 0, 0.06):- !. % (141/161)
same_sentence(n:male,  19, 0, 0.10):- !. % (117/147)
same_sentence(n:female,19, 0, 0.10):- !. % (117/147)
same_sentence(n:thing, 19, 0, 0.10):- !. % (117/147)
same_sentence(n:male,  20, 0, 0.05):- !. % (117/130)
same_sentence(n:female,20, 0, 0.05):- !. % (117/130)
same_sentence(n:thing, 20, 0, 0.05):- !. % (117/130)
same_sentence(n:male,  21, 0, 0.07):- !. % (92/108)
same_sentence(n:female,21, 0, 0.07):- !. % (92/108)
same_sentence(n:thing, 21, 0, 0.07):- !. % (92/108)
same_sentence(n:male,  22, 0, 0.06):- !. % (90/104)
same_sentence(n:female,22, 0, 0.06):- !. % (90/104)
same_sentence(n:thing, 22, 0, 0.06):- !. % (90/104)
same_sentence(n:male,  23, 0, 0.13):- !. % (75/102)
same_sentence(n:female,23, 0, 0.13):- !. % (75/102)
same_sentence(n:thing, 23, 0, 0.13):- !. % (75/102)
same_sentence(n:male,   _, 0, 0.07):- !. % (417/486)
same_sentence(n:female, _, 0, 0.07):- !. % (417/486)
same_sentence(n:thing,  _, 0, 0.07):- !. % (417/486)
same_sentence(n:male,  1, -1, 0.06):- !. % (832/963)
same_sentence(n:female,1, -1, 0.06):- !. % (832/963)
same_sentence(n:thing, 1, -1, 0.06):- !. % (832/963)
same_sentence(n:male,  2, -1, 0.10):- !. % (103/130)
same_sentence(n:female,2, -1, 0.10):- !. % (103/130)
same_sentence(n:thing, 2, -1, 0.10):- !. % (103/130)
same_sentence(n:male,  3, -1, 0.15):- !. % (126/180)
same_sentence(n:female,3, -1, 0.15):- !. % (126/180)
same_sentence(n:thing, 3, -1, 0.15):- !. % (126/180)
same_sentence(n:male,  4, -1, 0.35):- !. % (99/220)
same_sentence(n:female,4, -1, 0.35):- !. % (99/220)
same_sentence(n:thing, 4, -1, 0.35):- !. % (99/220)
same_sentence(n:male,  5, -1, 0.46):- !. % (62/178)
same_sentence(n:female,5, -1, 0.46):- !. % (62/178)
same_sentence(n:thing, 5, -1, 0.46):- !. % (62/178)
same_sentence(n:male,  6, -1, 0.54):- !. % (64/220)
same_sentence(n:female,6, -1, 0.54):- !. % (64/220)
same_sentence(n:thing, 6, -1, 0.54):- !. % (64/220)
same_sentence(n:male,  7, -1, 0.51):- !. % (60/195)
same_sentence(n:female,7, -1, 0.51):- !. % (60/195)
same_sentence(n:thing, 7, -1, 0.51):- !. % (60/195)
same_sentence(n:male,  8, -1, 0.55):- !. % (56/198)
same_sentence(n:female,8, -1, 0.55):- !. % (56/198)
same_sentence(n:thing, 8, -1, 0.55):- !. % (56/198)
same_sentence(n:male,  9, -1, 0.65):- !. % (39/176)
same_sentence(n:female,9, -1, 0.65):- !. % (39/176)
same_sentence(n:thing, 9, -1, 0.65):- !. % (39/176)
same_sentence(n:male,  10, -1, 0.59):- !. % (52/203)
same_sentence(n:female,10, -1, 0.59):- !. % (52/203)
same_sentence(n:thing, 10, -1, 0.59):- !. % (52/203)
same_sentence(n:male,  11, -1, 0.71):- !. % (37/190)
same_sentence(n:female,11, -1, 0.71):- !. % (37/190)
same_sentence(n:thing, 11, -1, 0.71):- !. % (37/190)
same_sentence(n:male,  12, -1, 0.67):- !. % (40/189)
same_sentence(n:female,12, -1, 0.67):- !. % (40/189)
same_sentence(n:thing, 12, -1, 0.67):- !. % (40/189)
same_sentence(n:male,  13, -1, 0.86):- !. % (24/173)
same_sentence(n:female,13, -1, 0.86):- !. % (24/173)
same_sentence(n:thing, 13, -1, 0.86):- !. % (24/173)
same_sentence(n:male,  14, -1, 0.76):- !. % (29/168)
same_sentence(n:female,14, -1, 0.76):- !. % (29/168)
same_sentence(n:thing, 14, -1, 0.76):- !. % (29/168)
same_sentence(n:male,  15, -1, 0.85):- !. % (27/193)
same_sentence(n:female,15, -1, 0.85):- !. % (27/193)
same_sentence(n:thing, 15, -1, 0.85):- !. % (27/193)
same_sentence(n:male,  16, -1, 0.92):- !. % (20/167)
same_sentence(n:female,16, -1, 0.92):- !. % (20/167)
same_sentence(n:thing, 16, -1, 0.92):- !. % (20/167)
same_sentence(n:male,  17, -1, 1.01):- !. % (16/162)
same_sentence(n:female,17, -1, 1.01):- !. % (16/162)
same_sentence(n:thing, 17, -1, 1.01):- !. % (16/162)
same_sentence(n:male,  18, -1, 1.00):- !. % (16/161)
same_sentence(n:female,18, -1, 1.00):- !. % (16/161)
same_sentence(n:thing, 18, -1, 1.00):- !. % (16/161)
same_sentence(n:male,  19, -1, 0.89):- !. % (19/147)
same_sentence(n:female,19, -1, 0.89):- !. % (19/147)
same_sentence(n:thing, 19, -1, 0.89):- !. % (19/147)
same_sentence(n:male,  20, -1, 1.07):- !. % (11/130)
same_sentence(n:female,20, -1, 1.07):- !. % (11/130)
same_sentence(n:thing, 20, -1, 1.07):- !. % (11/130)
same_sentence(n:male,  21, -1, 0.92):- !. % (13/108)
same_sentence(n:female,21, -1, 0.92):- !. % (13/108)
same_sentence(n:thing, 21, -1, 0.92):- !. % (13/108)
same_sentence(n:male,  22, -1, 1.02):- !. % (10/104)
same_sentence(n:female,22, -1, 1.02):- !. % (10/104)
same_sentence(n:thing, 22, -1, 1.02):- !. % (10/104)
same_sentence(n:male,  23, -1, 0.71):- !. % (20/102)
same_sentence(n:female,23, -1, 0.71):- !. % (20/102)
same_sentence(n:thing, 23, -1, 0.71):- !. % (20/102)
same_sentence(n:male,   _, -1, 0.99):- !. % (40/389)
same_sentence(n:female, _, -1, 0.99):- !. % (40/389)
same_sentence(n:thing,  _, -1, 0.99):- !. % (40/389)
same_sentence(n:male,  1, -2, 0.96):- !. % (106/963)
same_sentence(n:female,1, -2, 0.96):- !. % (106/963)
same_sentence(n:thing, 1, -2, 0.96):- !. % (106/963)
same_sentence(n:male,  2, -2, 0.81):- !. % (20/130)
same_sentence(n:female,2, -2, 0.81):- !. % (20/130)
same_sentence(n:thing, 2, -2, 0.81):- !. % (20/130)
same_sentence(n:male,  3, -2, 1.02):- !. % (17/180)
same_sentence(n:female,3, -2, 1.02):- !. % (17/180)
same_sentence(n:thing, 3, -2, 1.02):- !. % (17/180)
same_sentence(n:male,  4, -2, 0.98):- !. % (23/220)
same_sentence(n:female,4, -2, 0.98):- !. % (23/220)
same_sentence(n:thing, 4, -2, 0.98):- !. % (23/220)
same_sentence(n:male,  5, -2, 1.07):- !. % (15/178)
same_sentence(n:female,5, -2, 1.07):- !. % (15/178)
same_sentence(n:thing, 5, -2, 1.07):- !. % (15/178)
same_sentence(n:male,  6, -2, 1.39):- !. % (9/220)
same_sentence(n:female,6, -2, 1.39):- !. % (9/220)
same_sentence(n:thing, 6, -2, 1.39):- !. % (9/220)
same_sentence(n:male,  7, -2, 1.34):- !. % (9/195)
same_sentence(n:female,7, -2, 1.34):- !. % (9/195)
same_sentence(n:thing, 7, -2, 1.34):- !. % (9/195)
same_sentence(n:male,  8, -2, 1.30):- !. % (10/198)
same_sentence(n:female,8, -2, 1.30):- !. % (10/198)
same_sentence(n:thing, 8, -2, 1.30):- !. % (10/198)
same_sentence(n:male,  9, -2, 1.29):- !. % (9/176)
same_sentence(n:female,9, -2, 1.29):- !. % (9/176)
same_sentence(n:thing, 9, -2, 1.29):- !. % (9/176)
same_sentence(n:male,  10, -2, 1.46):- !. % (7/203)
same_sentence(n:female,10, -2, 1.46):- !. % (7/203)
same_sentence(n:thing, 10, -2, 1.46):- !. % (7/203)
same_sentence(n:male,  11, -2, 1.38):- !. % (8/190)
same_sentence(n:female,11, -2, 1.38):- !. % (8/190)
same_sentence(n:thing, 11, -2, 1.38):- !. % (8/190)
same_sentence(n:male,  12, -2, 1.80):- !. % (3/189)
same_sentence(n:female,12, -2, 1.80):- !. % (3/189)
same_sentence(n:thing, 12, -2, 1.80):- !. % (3/189)
same_sentence(n:male,  13, -2, 1.20):- !. % (11/173)
same_sentence(n:female,13, -2, 1.20):- !. % (11/173)
same_sentence(n:thing, 13, -2, 1.20):- !. % (11/173)
same_sentence(n:male,  14, -2, 1.53):- !. % (5/168)
same_sentence(n:female,14, -2, 1.53):- !. % (5/168)
same_sentence(n:thing, 14, -2, 1.53):- !. % (5/168)
same_sentence(n:male,  15, -2, 1.24):- !. % (11/193)
same_sentence(n:female,15, -2, 1.24):- !. % (11/193)
same_sentence(n:thing, 15, -2, 1.24):- !. % (11/193)
same_sentence(n:male,  16, -2, 1.38):- !. % (7/167)
same_sentence(n:female,16, -2, 1.38):- !. % (7/167)
same_sentence(n:thing, 16, -2, 1.38):- !. % (7/167)
same_sentence(n:male,  17, -2, 1.51):- !. % (5/162)
same_sentence(n:female,17, -2, 1.51):- !. % (5/162)
same_sentence(n:thing, 17, -2, 1.51):- !. % (5/162)
same_sentence(n:male,  18, -2, 1.73):- !. % (3/161)
same_sentence(n:female,18, -2, 1.73):- !. % (3/161)
same_sentence(n:thing, 18, -2, 1.73):- !. % (3/161)
same_sentence(n:male,  19, -2, 1.21):- !. % (9/147)
same_sentence(n:female,19, -2, 1.21):- !. % (9/147)
same_sentence(n:thing, 19, -2, 1.21):- !. % (9/147)
same_sentence(n:male,  20, -2, 2.11):- !. % (1/130)
same_sentence(n:female,20, -2, 2.11):- !. % (1/130)
same_sentence(n:thing, 20, -2, 2.11):- !. % (1/130)
same_sentence(n:male,  21, -2, 2.03):- !. % (1/108)
same_sentence(n:female,21, -2, 2.03):- !. % (1/108)
same_sentence(n:thing, 21, -2, 2.03):- !. % (1/108)
same_sentence(n:male,  22, -2, 1.72):- !. % (2/104)
same_sentence(n:female,22, -2, 1.72):- !. % (2/104)
same_sentence(n:thing, 22, -2, 1.72):- !. % (2/104)
same_sentence(n:male,  23, -2, 1.41):- !. % (4/102)
same_sentence(n:female,23, -2, 1.41):- !. % (4/102)
same_sentence(n:thing, 23, -2, 1.41):- !. % (4/102)
same_sentence(n:male,   _, -2, 1.27):- !. % (4/75)
same_sentence(n:female, _, -2, 1.27):- !. % (4/75)
same_sentence(n:thing,  _, -2, 1.27):- !. % (4/75)
same_sentence(n:male,  1, -3, 1.70):- !. % (19/963)
same_sentence(n:female,1, -3, 1.70):- !. % (19/963)
same_sentence(n:thing, 1, -3, 1.70):- !. % (19/963)
same_sentence(n:male,  2, -3, 1.51):- !. % (4/130)
same_sentence(n:female,2, -3, 1.51):- !. % (4/130)
same_sentence(n:thing, 2, -3, 1.51):- !. % (4/130)
same_sentence(n:male,  3, -3, 1.78):- !. % (3/180)
same_sentence(n:female,3, -3, 1.78):- !. % (3/180)
same_sentence(n:thing, 3, -3, 1.78):- !. % (3/180)
same_sentence(n:male,  4, -3, 2.04):- !. % (2/220)
same_sentence(n:female,4, -3, 2.04):- !. % (2/220)
same_sentence(n:thing, 4, -3, 2.04):- !. % (2/220)
same_sentence(n:male,  5, -3, 1.35):- !. % (8/178)
same_sentence(n:female,5, -3, 1.35):- !. % (8/178)
same_sentence(n:thing, 5, -3, 1.35):- !. % (8/178)
same_sentence(n:male,  6, -3, 2.04):- !. % (2/220)
same_sentence(n:female,6, -3, 2.04):- !. % (2/220)
same_sentence(n:thing, 6, -3, 2.04):- !. % (2/220)
same_sentence(n:male,  7, -3, 1.51):- !. % (6/195)
same_sentence(n:female,7, -3, 1.51):- !. % (6/195)
same_sentence(n:thing, 7, -3, 1.51):- !. % (6/195)
same_sentence(n:male,  8, -3, 2.00):- !. % (2/198)
same_sentence(n:female,8, -3, 2.00):- !. % (2/198)
same_sentence(n:thing, 8, -3, 2.00):- !. % (2/198)
same_sentence(n:male,  9, -3, 2.25):- !. % (1/176)
same_sentence(n:female,9, -3, 2.25):- !. % (1/176)
same_sentence(n:thing, 9, -3, 2.25):- !. % (1/176)
same_sentence(n:male,  10, -3, 1.61):- !. % (5/203)
same_sentence(n:female,10, -3, 1.61):- !. % (5/203)
same_sentence(n:thing, 10, -3, 1.61):- !. % (5/203)
same_sentence(n:male,  11, -3, 1.98):- !. % (2/190)
same_sentence(n:female,11, -3, 1.98):- !. % (2/190)
same_sentence(n:thing, 11, -3, 1.98):- !. % (2/190)
same_sentence(n:male,  12, -3, 10.00):- !. % (0/189)
same_sentence(n:female,12, -3, 10.00):- !. % (0/189)
same_sentence(n:thing, 12, -3, 10.00):- !. % (0/189)
same_sentence(n:male,  13, -3, 1.64):- !. % (4/173)
same_sentence(n:female,13, -3, 1.64):- !. % (4/173)
same_sentence(n:thing, 13, -3, 1.64):- !. % (4/173)
same_sentence(n:male,  14, -3, 10.00):- !. % (0/168)
same_sentence(n:female,14, -3, 10.00):- !. % (0/168)
same_sentence(n:thing, 14, -3, 10.00):- !. % (0/168)
same_sentence(n:male,  15, -3, 10.00):- !. % (0/193)
same_sentence(n:female,15, -3, 10.00):- !. % (0/193)
same_sentence(n:thing, 15, -3, 10.00):- !. % (0/193)
same_sentence(n:male,  16, -3, 10.00):- !. % (0/167)
same_sentence(n:female,16, -3, 10.00):- !. % (0/167)
same_sentence(n:thing, 16, -3, 10.00):- !. % (0/167)
same_sentence(n:male,  17, -3, 10.00):- !. % (0/162)
same_sentence(n:female,17, -3, 10.00):- !. % (0/162)
same_sentence(n:thing, 17, -3, 10.00):- !. % (0/162)
same_sentence(n:male,  18, -3, 10.00):- !. % (0/161)
same_sentence(n:female,18, -3, 10.00):- !. % (0/161)
same_sentence(n:thing, 18, -3, 10.00):- !. % (0/161)
same_sentence(n:male,  19, -3, 1.87):- !. % (2/147)
same_sentence(n:female,19, -3, 1.87):- !. % (2/147)
same_sentence(n:thing, 19, -3, 1.87):- !. % (2/147)
same_sentence(n:male,  20, -3, 2.11):- !. % (1/130)
same_sentence(n:female,20, -3, 2.11):- !. % (1/130)
same_sentence(n:thing, 20, -3, 2.11):- !. % (1/130)
same_sentence(n:male,  21, -3, 2.03):- !. % (1/108)
same_sentence(n:female,21, -3, 2.03):- !. % (1/108)
same_sentence(n:thing, 21, -3, 2.03):- !. % (1/108)
same_sentence(n:male,  22, -3, 2.02):- !. % (1/104)
same_sentence(n:female,22, -3, 2.02):- !. % (1/104)
same_sentence(n:thing, 22, -3, 2.02):- !. % (1/104)
same_sentence(n:male,  23, -3, 1.53):- !. % (3/102)
same_sentence(n:female,23, -3, 1.53):- !. % (3/102)
same_sentence(n:thing, 23, -3, 1.53):- !. % (3/102)
same_sentence(n:male,  1, -4, 2.28):- !. % (5/963)
same_sentence(n:female,1, -4, 2.28):- !. % (5/963)
same_sentence(n:thing, 1, -4, 2.28):- !. % (5/963)
same_sentence(n:male,  2, -4, 2.11):- !. % (1/130)
same_sentence(n:female,2, -4, 2.11):- !. % (1/130)
same_sentence(n:thing, 2, -4, 2.11):- !. % (1/130)
same_sentence(n:male,  3, -4, 1.95):- !. % (2/180)
same_sentence(n:female,3, -4, 1.95):- !. % (2/180)
same_sentence(n:thing, 3, -4, 1.95):- !. % (2/180)
same_sentence(n:male,  4, -4, 10.00):- !. % (0/220)
same_sentence(n:female,4, -4, 10.00):- !. % (0/220)
same_sentence(n:thing, 4, -4, 10.00):- !. % (0/220)
same_sentence(n:male,  5, -4, 10.00):- !. % (0/178)
same_sentence(n:female,5, -4, 10.00):- !. % (0/178)
same_sentence(n:thing, 5, -4, 10.00):- !. % (0/178)
same_sentence(n:male,  6, -4, 1.87):- !. % (3/220)
same_sentence(n:female,6, -4, 1.87):- !. % (3/220)
same_sentence(n:thing, 6, -4, 1.87):- !. % (3/220)
same_sentence(n:male,  7, -4, 2.29):- !. % (1/195)
same_sentence(n:female,7, -4, 2.29):- !. % (1/195)
same_sentence(n:thing, 7, -4, 2.29):- !. % (1/195)
same_sentence(n:male,  8, -4, 2.00):- !. % (2/198)
same_sentence(n:female,8, -4, 2.00):- !. % (2/198)
same_sentence(n:thing, 8, -4, 2.00):- !. % (2/198)
same_sentence(n:male,  9, -4, 10.00):- !. % (0/176)
same_sentence(n:female,9, -4, 10.00):- !. % (0/176)
same_sentence(n:thing, 9, -4, 10.00):- !. % (0/176)
same_sentence(n:male,  10, -4, 2.31):- !. % (1/203)
same_sentence(n:female,10, -4, 2.31):- !. % (1/203)
same_sentence(n:thing, 10, -4, 2.31):- !. % (1/203)
same_sentence(n:male,  11, -4, 2.28):- !. % (1/190)
same_sentence(n:female,11, -4, 2.28):- !. % (1/190)
same_sentence(n:thing, 11, -4, 2.28):- !. % (1/190)
same_sentence(n:male,  12, -4, 10.00):- !. % (0/189)
same_sentence(n:female,12, -4, 10.00):- !. % (0/189)
same_sentence(n:thing, 12, -4, 10.00):- !. % (0/189)
same_sentence(n:male,  13, -4, 2.24):- !. % (1/173)
same_sentence(n:female,13, -4, 2.24):- !. % (1/173)
same_sentence(n:thing, 13, -4, 2.24):- !. % (1/173)
same_sentence(n:male,  14, -4, 10.00):- !. % (0/168)
same_sentence(n:female,14, -4, 10.00):- !. % (0/168)
same_sentence(n:thing, 14, -4, 10.00):- !. % (0/168)
same_sentence(n:male,  15, -4, 2.29):- !. % (1/193)
same_sentence(n:female,15, -4, 2.29):- !. % (1/193)
same_sentence(n:thing, 15, -4, 2.29):- !. % (1/193)
same_sentence(n:male,  16, -4, 10.00):- !. % (0/167)
same_sentence(n:female,16, -4, 10.00):- !. % (0/167)
same_sentence(n:thing, 16, -4, 10.00):- !. % (0/167)
same_sentence(n:male,  17, -4, 10.00):- !. % (0/162)
same_sentence(n:female,17, -4, 10.00):- !. % (0/162)
same_sentence(n:thing, 17, -4, 10.00):- !. % (0/162)
same_sentence(n:male,  18, -4, 10.00):- !. % (0/161)
same_sentence(n:female,18, -4, 10.00):- !. % (0/161)
same_sentence(n:thing, 18, -4, 10.00):- !. % (0/161)
same_sentence(n:male,  19, -4, 10.00):- !. % (0/147)
same_sentence(n:female,19, -4, 10.00):- !. % (0/147)
same_sentence(n:thing, 19, -4, 10.00):- !. % (0/147)
same_sentence(n:male,  20, -4, 10.00):- !. % (0/130)
same_sentence(n:female,20, -4, 10.00):- !. % (0/130)
same_sentence(n:thing, 20, -4, 10.00):- !. % (0/130)
same_sentence(n:male,  21, -4, 2.03):- !. % (1/108)
same_sentence(n:female,21, -4, 2.03):- !. % (1/108)
same_sentence(n:thing, 21, -4, 2.03):- !. % (1/108)
same_sentence(n:male,  22, -4, 2.02):- !. % (1/104)
same_sentence(n:female,22, -4, 2.02):- !. % (1/104)
same_sentence(n:thing, 22, -4, 2.02):- !. % (1/104)
same_sentence(n:male,  23, -4, 10.00):- !. % (0/102)
same_sentence(n:female,23, -4, 10.00):- !. % (0/102)
same_sentence(n:thing, 23, -4, 10.00):- !. % (0/102)
same_sentence(n:male,  1, -5, 10.00):- !. % (0/963)
same_sentence(n:female,1, -5, 10.00):- !. % (0/963)
same_sentence(n:thing, 1, -5, 10.00):- !. % (0/963)
same_sentence(n:male,  2, -5, 1.81):- !. % (2/130)
same_sentence(n:female,2, -5, 1.81):- !. % (2/130)
same_sentence(n:thing, 2, -5, 1.81):- !. % (2/130)
same_sentence(n:male,  3, -5, 10.00):- !. % (0/180)
same_sentence(n:female,3, -5, 10.00):- !. % (0/180)
same_sentence(n:thing, 3, -5, 10.00):- !. % (0/180)
same_sentence(n:male,  4, -5, 10.00):- !. % (0/220)
same_sentence(n:female,4, -5, 10.00):- !. % (0/220)
same_sentence(n:thing, 4, -5, 10.00):- !. % (0/220)
same_sentence(n:male,  5, -5, 2.25):- !. % (1/178)
same_sentence(n:female,5, -5, 2.25):- !. % (1/178)
same_sentence(n:thing, 5, -5, 2.25):- !. % (1/178)
same_sentence(n:male,  6, -5, 2.34):- !. % (1/220)
same_sentence(n:female,6, -5, 2.34):- !. % (1/220)
same_sentence(n:thing, 6, -5, 2.34):- !. % (1/220)
same_sentence(n:male,  7, -5, 2.29):- !. % (1/195)
same_sentence(n:female,7, -5, 2.29):- !. % (1/195)
same_sentence(n:thing, 7, -5, 2.29):- !. % (1/195)
same_sentence(n:male,  8, -5, 10.00):- !. % (0/198)
same_sentence(n:female,8, -5, 10.00):- !. % (0/198)
same_sentence(n:thing, 8, -5, 10.00):- !. % (0/198)
same_sentence(n:male,  9, -5, 10.00):- !. % (0/176)
same_sentence(n:female,9, -5, 10.00):- !. % (0/176)
same_sentence(n:thing, 9, -5, 10.00):- !. % (0/176)
same_sentence(n:male,  10, -5, 10.00):- !. % (0/203)
same_sentence(n:female,10, -5, 10.00):- !. % (0/203)
same_sentence(n:thing, 10, -5, 10.00):- !. % (0/203)
same_sentence(n:male,  11, -5, 10.00):- !. % (0/190)
same_sentence(n:female,11, -5, 10.00):- !. % (0/190)
same_sentence(n:thing, 11, -5, 10.00):- !. % (0/190)
same_sentence(n:male,  12, -5, 10.00):- !. % (0/189)
same_sentence(n:female,12, -5, 10.00):- !. % (0/189)
same_sentence(n:thing, 12, -5, 10.00):- !. % (0/189)
same_sentence(n:male,  13, -5, 2.24):- !. % (1/173)
same_sentence(n:female,13, -5, 2.24):- !. % (1/173)
same_sentence(n:thing, 13, -5, 2.24):- !. % (1/173)
same_sentence(n:male,  14, -5, 10.00):- !. % (0/168)
same_sentence(n:female,14, -5, 10.00):- !. % (0/168)
same_sentence(n:thing, 14, -5, 10.00):- !. % (0/168)
same_sentence(n:male,  15, -5, 10.00):- !. % (0/193)
same_sentence(n:female,15, -5, 10.00):- !. % (0/193)
same_sentence(n:thing, 15, -5, 10.00):- !. % (0/193)
same_sentence(n:male,  16, -5, 10.00):- !. % (0/167)
same_sentence(n:female,16, -5, 10.00):- !. % (0/167)
same_sentence(n:thing, 16, -5, 10.00):- !. % (0/167)
same_sentence(n:male,  17, -5, 2.21):- !. % (1/162)
same_sentence(n:female,17, -5, 2.21):- !. % (1/162)
same_sentence(n:thing, 17, -5, 2.21):- !. % (1/162)
same_sentence(n:male,  18, -5, 10.00):- !. % (0/161)
same_sentence(n:female,18, -5, 10.00):- !. % (0/161)
same_sentence(n:thing, 18, -5, 10.00):- !. % (0/161)
same_sentence(n:male,  19, -5, 10.00):- !. % (0/147)
same_sentence(n:female,19, -5, 10.00):- !. % (0/147)
same_sentence(n:thing, 19, -5, 10.00):- !. % (0/147)
same_sentence(n:male,  20, -5, 10.00):- !. % (0/130)
same_sentence(n:female,20, -5, 10.00):- !. % (0/130)
same_sentence(n:thing, 20, -5, 10.00):- !. % (0/130)
same_sentence(n:male,  21, -5, 10.00):- !. % (0/108)
same_sentence(n:female,21, -5, 10.00):- !. % (0/108)
same_sentence(n:thing, 21, -5, 10.00):- !. % (0/108)
same_sentence(n:male,  22, -5, 10.00):- !. % (0/104)
same_sentence(n:female,22, -5, 10.00):- !. % (0/104)
same_sentence(n:thing, 22, -5, 10.00):- !. % (0/104)
same_sentence(n:male,  23, -5, 10.00):- !. % (0/102)
same_sentence(n:female,23, -5, 10.00):- !. % (0/102)
same_sentence(n:thing, 23, -5, 10.00):- !. % (0/102)
same_sentence(n:male,  1, -6, 2.98):- !. % (1/963)
same_sentence(n:female,1, -6, 2.98):- !. % (1/963)
same_sentence(n:thing, 1, -6, 2.98):- !. % (1/963)
same_sentence(n:male,  2, -6, 10.00):- !. % (0/130)
same_sentence(n:female,2, -6, 10.00):- !. % (0/130)
same_sentence(n:thing, 2, -6, 10.00):- !. % (0/130)
same_sentence(n:male,  3, -6, 10.00):- !. % (0/180)
same_sentence(n:female,3, -6, 10.00):- !. % (0/180)
same_sentence(n:thing, 3, -6, 10.00):- !. % (0/180)
same_sentence(n:male,  4, -6, 10.00):- !. % (0/220)
same_sentence(n:female,4, -6, 10.00):- !. % (0/220)
same_sentence(n:thing, 4, -6, 10.00):- !. % (0/220)
same_sentence(n:male,  5, -6, 2.25):- !. % (1/178)
same_sentence(n:female,5, -6, 2.25):- !. % (1/178)
same_sentence(n:thing, 5, -6, 2.25):- !. % (1/178)
same_sentence(n:male,  6, -6, 2.34):- !. % (1/220)
same_sentence(n:female,6, -6, 2.34):- !. % (1/220)
same_sentence(n:thing, 6, -6, 2.34):- !. % (1/220)
same_sentence(n:male,  7, -6, 10.00):- !. % (0/195)
same_sentence(n:female,7, -6, 10.00):- !. % (0/195)
same_sentence(n:thing, 7, -6, 10.00):- !. % (0/195)
same_sentence(n:male,  8, -6, 10.00):- !. % (0/198)
same_sentence(n:female,8, -6, 10.00):- !. % (0/198)
same_sentence(n:thing, 8, -6, 10.00):- !. % (0/198)
same_sentence(n:male,  9, -6, 10.00):- !. % (0/176)
same_sentence(n:female,9, -6, 10.00):- !. % (0/176)
same_sentence(n:thing, 9, -6, 10.00):- !. % (0/176)
same_sentence(n:male,  10, -6, 10.00):- !. % (0/203)
same_sentence(n:female,10, -6, 10.00):- !. % (0/203)
same_sentence(n:thing, 10, -6, 10.00):- !. % (0/203)
same_sentence(n:male,  11, -6, 2.28):- !. % (1/190)
same_sentence(n:female,11, -6, 2.28):- !. % (1/190)
same_sentence(n:thing, 11, -6, 2.28):- !. % (1/190)
same_sentence(n:male,  12, -6, 10.00):- !. % (0/189)
same_sentence(n:female,12, -6, 10.00):- !. % (0/189)
same_sentence(n:thing, 12, -6, 10.00):- !. % (0/189)
same_sentence(n:male,  13, -6, 10.00):- !. % (0/173)
same_sentence(n:female,13, -6, 10.00):- !. % (0/173)
same_sentence(n:thing, 13, -6, 10.00):- !. % (0/173)
same_sentence(n:male,  14, -6, 10.00):- !. % (0/168)
same_sentence(n:female,14, -6, 10.00):- !. % (0/168)
same_sentence(n:thing, 14, -6, 10.00):- !. % (0/168)
same_sentence(n:male,  15, -6, 10.00):- !. % (0/193)
same_sentence(n:female,15, -6, 10.00):- !. % (0/193)
same_sentence(n:thing, 15, -6, 10.00):- !. % (0/193)
same_sentence(n:male,  16, -6, 10.00):- !. % (0/167)
same_sentence(n:female,16, -6, 10.00):- !. % (0/167)
same_sentence(n:thing, 16, -6, 10.00):- !. % (0/167)
same_sentence(n:male,  17, -6, 10.00):- !. % (0/162)
same_sentence(n:female,17, -6, 10.00):- !. % (0/162)
same_sentence(n:thing, 17, -6, 10.00):- !. % (0/162)
same_sentence(n:male,  18, -6, 2.21):- !. % (1/161)
same_sentence(n:female,18, -6, 2.21):- !. % (1/161)
same_sentence(n:thing, 18, -6, 2.21):- !. % (1/161)
same_sentence(n:male,  19, -6, 10.00):- !. % (0/147)
same_sentence(n:female,19, -6, 10.00):- !. % (0/147)
same_sentence(n:thing, 19, -6, 10.00):- !. % (0/147)
same_sentence(n:male,  20, -6, 10.00):- !. % (0/130)
same_sentence(n:female,20, -6, 10.00):- !. % (0/130)
same_sentence(n:thing, 20, -6, 10.00):- !. % (0/130)
same_sentence(n:male,  21, -6, 10.00):- !. % (0/108)
same_sentence(n:female,21, -6, 10.00):- !. % (0/108)
same_sentence(n:thing, 21, -6, 10.00):- !. % (0/108)
same_sentence(n:male,  22, -6, 10.00):- !. % (0/104)
same_sentence(n:female,22, -6, 10.00):- !. % (0/104)
same_sentence(n:thing, 22, -6, 10.00):- !. % (0/104)
same_sentence(n:male,  23, -6, 10.00):- !. % (0/102)
same_sentence(n:female,23, -6, 10.00):- !. % (0/102)
same_sentence(n:thing, 23, -6, 10.00):- !. % (0/102)
same_sentence(n:male,  1, -7, 10.00):- !. % (0/963)
same_sentence(n:female,1, -7, 10.00):- !. % (0/963)
same_sentence(n:thing, 1, -7, 10.00):- !. % (0/963)
same_sentence(n:male,  2, -7, 10.00):- !. % (0/130)
same_sentence(n:female,2, -7, 10.00):- !. % (0/130)
same_sentence(n:thing, 2, -7, 10.00):- !. % (0/130)
same_sentence(n:male,  3, -7, 10.00):- !. % (0/180)
same_sentence(n:female,3, -7, 10.00):- !. % (0/180)
same_sentence(n:thing, 3, -7, 10.00):- !. % (0/180)
same_sentence(n:male,  4, -7, 10.00):- !. % (0/220)
same_sentence(n:female,4, -7, 10.00):- !. % (0/220)
same_sentence(n:thing, 4, -7, 10.00):- !. % (0/220)
same_sentence(n:male,  5, -7, 10.00):- !. % (0/178)
same_sentence(n:female,5, -7, 10.00):- !. % (0/178)
same_sentence(n:thing, 5, -7, 10.00):- !. % (0/178)
same_sentence(n:male,  6, -7, 2.34):- !. % (1/220)
same_sentence(n:female,6, -7, 2.34):- !. % (1/220)
same_sentence(n:thing, 6, -7, 2.34):- !. % (1/220)
same_sentence(n:male,  7, -7, 10.00):- !. % (0/195)
same_sentence(n:female,7, -7, 10.00):- !. % (0/195)
same_sentence(n:thing, 7, -7, 10.00):- !. % (0/195)
same_sentence(n:male,  8, -7, 10.00):- !. % (0/198)
same_sentence(n:female,8, -7, 10.00):- !. % (0/198)
same_sentence(n:thing, 8, -7, 10.00):- !. % (0/198)
same_sentence(n:male,  9, -7, 10.00):- !. % (0/176)
same_sentence(n:female,9, -7, 10.00):- !. % (0/176)
same_sentence(n:thing, 9, -7, 10.00):- !. % (0/176)
same_sentence(n:male,  10, -7, 10.00):- !. % (0/203)
same_sentence(n:female,10, -7, 10.00):- !. % (0/203)
same_sentence(n:thing, 10, -7, 10.00):- !. % (0/203)
same_sentence(n:male,  11, -7, 10.00):- !. % (0/190)
same_sentence(n:female,11, -7, 10.00):- !. % (0/190)
same_sentence(n:thing, 11, -7, 10.00):- !. % (0/190)
same_sentence(n:male,  12, -7, 10.00):- !. % (0/189)
same_sentence(n:female,12, -7, 10.00):- !. % (0/189)
same_sentence(n:thing, 12, -7, 10.00):- !. % (0/189)
same_sentence(n:male,  13, -7, 2.24):- !. % (1/173)
same_sentence(n:female,13, -7, 2.24):- !. % (1/173)
same_sentence(n:thing, 13, -7, 2.24):- !. % (1/173)
same_sentence(n:male,  14, -7, 10.00):- !. % (0/168)
same_sentence(n:female,14, -7, 10.00):- !. % (0/168)
same_sentence(n:thing, 14, -7, 10.00):- !. % (0/168)
same_sentence(n:male,  15, -7, 10.00):- !. % (0/193)
same_sentence(n:female,15, -7, 10.00):- !. % (0/193)
same_sentence(n:thing, 15, -7, 10.00):- !. % (0/193)
same_sentence(n:male,  16, -7, 2.22):- !. % (1/167)
same_sentence(n:female,16, -7, 2.22):- !. % (1/167)
same_sentence(n:thing, 16, -7, 2.22):- !. % (1/167)
same_sentence(n:male,  17, -7, 10.00):- !. % (0/162)
same_sentence(n:female,17, -7, 10.00):- !. % (0/162)
same_sentence(n:thing, 17, -7, 10.00):- !. % (0/162)
same_sentence(n:male,  18, -7, 10.00):- !. % (0/161)
same_sentence(n:female,18, -7, 10.00):- !. % (0/161)
same_sentence(n:thing, 18, -7, 10.00):- !. % (0/161)
same_sentence(n:male,  19, -7, 10.00):- !. % (0/147)
same_sentence(n:female,19, -7, 10.00):- !. % (0/147)
same_sentence(n:thing, 19, -7, 10.00):- !. % (0/147)
same_sentence(n:male,  20, -7, 10.00):- !. % (0/130)
same_sentence(n:female,20, -7, 10.00):- !. % (0/130)
same_sentence(n:thing, 20, -7, 10.00):- !. % (0/130)
same_sentence(n:male,  21, -7, 10.00):- !. % (0/108)
same_sentence(n:female,21, -7, 10.00):- !. % (0/108)
same_sentence(n:thing, 21, -7, 10.00):- !. % (0/108)
same_sentence(n:male,  22, -7, 10.00):- !. % (0/104)
same_sentence(n:female,22, -7, 10.00):- !. % (0/104)
same_sentence(n:thing, 22, -7, 10.00):- !. % (0/104)
same_sentence(n:male,  23, -7, 10.00):- !. % (0/102)
same_sentence(n:female,23, -7, 10.00):- !. % (0/102)
same_sentence(n:thing, 23, -7, 10.00):- !. % (0/102)
same_sentence(n:male,  _, _, 10.00):- !. % 
same_sentence(n:female,_, _, 10.00):- !. % 
same_sentence(n:male,  _, _, 10.00):- !. % 
same_sentence(_,       _, _, 0.00):- !. % 

sentence_position_ant(1,1.048):- !. % F=461
sentence_position_ant(2,0.527):- !. % F=1528
sentence_position_ant(3,1.101):- !. % F=408
sentence_position_ant(4,0.963):- !. % F=560
sentence_position_ant(5,1.145):- !. % F=369
sentence_position_ant(6,1.290):- !. % F=264
sentence_position_ant(7,1.426):- !. % F=193
sentence_position_ant(8,1.419):- !. % F=196
sentence_position_ant(9,1.404):- !. % F=203
sentence_position_ant(10,1.547):- !. % F=146
sentence_position_ant(11,1.651):- !. % F=115
sentence_position_ant(12,1.629):- !. % F=121
sentence_position_ant(13,1.743):- !. % F=93
sentence_position_ant(14,1.808):- !. % F=80
sentence_position_ant(15,1.848):- !. % F=73
sentence_position_ant(16,1.979):- !. % F=54
sentence_position_ant(17,1.912):- !. % F=63
sentence_position_ant(18,2.068):- !. % F=44
sentence_position_ant(19,2.314):- !. % F=25
sentence_position_ant(20,2.193):- !. % F=33
sentence_position_ant(21,2.433):- !. % F=19
sentence_position_ant(22,2.220):- !. % F=31
sentence_position_ant(23,2.712):- !. % F=10
sentence_position_ant(24,2.866):- !. % F=7
sentence_position_ant(25,2.670):- !. % F=11
sentence_position_ant(26,3.109):- !. % F=4
sentence_position_ant(27,3.013):- !. % F=5
sentence_position_ant(28,2.808):- !. % F=8
sentence_position_ant(29,2.866):- !. % F=7
sentence_position_ant(30,3.712):- !. % F=1
sentence_position_ant(31,3.109):- !. % F=4
sentence_position_ant(32,2.933):- !. % F=6
sentence_position_ant(33,3.411):- !. % F=2
sentence_position_ant(34,3.712):- !. % F=1
sentence_position_ant( _,10).     % 

pos_ant('NNPS',3.23):- !. % F=3
pos_ant('DT',3.71):- !. % F=1
pos_ant('NNS',2.93):- !. % F=6
pos_ant('CD',2.67):- !. % F=11
pos_ant('NNP',0.19):- !. % F=3332
pos_ant('NN',0.46):- !. % F=1794
pos_ant(_,10):- !.

extractFeaturesAna([],[]).
extractFeaturesAna([pred(_,Sym,Pos,_)|L],[Pos:Sym|F]):- Pos=n, !, extractFeaturesAna(L,F).
extractFeaturesAna([named(_,Sym,Pos,_)|L],[Pos:Sym|F]):- !, extractFeaturesAna(L,F).
extractFeaturesAna([_:X|L],F):- !, extractFeaturesAna([X|L],F).
extractFeaturesAna([_|L],F):- extractFeaturesAna(L,F).

extractFeaturesAnt([],[]).
extractFeaturesAnt([pred(_,Sym,Pos,_)|L],[symb:Sym,sort:Pos|F]):- !, extractFeaturesAnt(L,F).
extractFeaturesAnt([named(_,Sym,Sort,Type)|L],[symb:Sym,sort:Sort,type:Type|F]):- !, extractFeaturesAnt(L,F).
%extractFeaturesAnt([role(_,_,Role,_)|L],[role:Role|F]):- !, extractFeaturesAnt(L,F).
extractFeaturesAnt([_:X|L],F):- !, extractFeaturesAnt([X|L],F).
extractFeaturesAnt([_|L],F):- extractFeaturesAnt(L,F).
