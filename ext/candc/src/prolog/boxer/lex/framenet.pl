
:- module(framenet,[fnpattern/4]).

fnpattern(immerse, 9010000, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(immerse, 9010000, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(lodge, 9010000, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(lodge, 9010000, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(position, 9010000, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(position, 9010000, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(situate, 9010000, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(situate, 9010000, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(deposit, 9010100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(deposit, 9010100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(insert, 9010100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(insert, 9010100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(stash, 9010100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(stash, 9010100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(stow, 9010100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(stow, 9010100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(place, 9010200, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(place, 9010200, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(put, 9010200, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(put, 9010200, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(set, 9010200, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(set, 9010200, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(lay, 9020000, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(lay, 9020000, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(hang, 9020100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(hang, 9020100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(lean, 9020100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(lean, 9020100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(perch, 9020100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(perch, 9020100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(rest, 9020100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(rest, 9020100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(sit, 9020100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(sit, 9020100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(stand, 9020100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(stand, 9020100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(tuck, 9030200, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme', 'Cause': 'Cause']).
fnpattern(ram, 9030210, 'Cause_impact', ['Agent': 'Agent', 'Theme': 'Impactor', 'Destination': 'Impactee']).
fnpattern(dribble, 9050000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(dribble, 9050000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(drip, 9050000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(drip, 9050000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(spew, 9050000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(spew, 9050000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(spill, 9050000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(spill, 9050000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(spin, 9060000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(spin, 9060000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(spin, 9060000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(spin, 9060000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(twirl, 9060100, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(twirl, 9060100, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(twirl, 9060100, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(twirl, 9060100, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(brush, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(brush, 9070100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(brush, 9070100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(drizzle, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(drizzle, 9070100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(drizzle, 9070100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(hang, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(hang, 9070100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(hang, 9070100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(plaster, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(pump, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(rub, 9070100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(rub, 9070100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(scatter, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(seed, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(shower, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(shower, 9070100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(shower, 9070100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(smear, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(smear, 9070100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(smear, 9070100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(sow, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(spatter, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(splash, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(splatter, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(spray, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(spread, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(sprinkle, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(squirt, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(stick, 9070100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(stick, 9070100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(strew, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(wrap, 9070100, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(wrap, 9070100, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(wrap, 9070100, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(cram, 9070110, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(cram, 9070110, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(cram, 9070110, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(crowd, 9070110, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(jam, 9070110, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(jam, 9070110, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(jam, 9070110, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(pack, 9070110, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(pack, 9070110, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(pack, 9070110, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(pile, 9070110, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(pile, 9070110, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(pile, 9070110, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(dab, 9070200, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(dab, 9070200, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(dab, 9070200, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(daub, 9070200, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(daub, 9070200, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(daub, 9070200, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(drape, 9070200, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(drape, 9070200, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(drape, 9070200, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(dust, 9070200, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(dust, 9070200, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(dust, 9070200, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(heap, 9070200, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(heap, 9070200, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(heap, 9070200, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(load, 9070200, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(load, 9070200, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(load, 9070200, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(adorn, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(adorn, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(anoint, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(blanket, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(cloak, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(coat, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(coat, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(cover, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(cover, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(deck, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(decorate, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(dot, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(douse, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(embellish, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(encircle, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(festoon, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(fill, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(fill, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(flood, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(inject, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(inject, 9080000, 'Placing', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(inject, 9080000, 'Placing', ['Agent': 'Cause', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(line, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(pave, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(pave, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(stud, 9080000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(suffuse, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(tile, 9080000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(asphalt, 9090000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(blanket, 9090000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(butter, 9090000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(cloak, 9090000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(panel, 9090000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(plaster, 9090000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(seed, 9090000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(ticket, 9090000, 'Fining', ['Agent': 'Speaker', 'Destination': 'Payer', 'Theme': 'Fine']).
fnpattern(tile, 9090000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(wallpaper, 9090000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(wreathe, 9090000, 'Adorning', ['Destination': 'Location', 'Theme': 'Theme']).
fnpattern(archive, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(archive, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(bag, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(bag, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(billet, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(billet, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(bin, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(bin, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(bottle, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(bottle, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(box, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(box, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(cage, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(cage, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(crate, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(crate, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(file, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(file, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(garage, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(garage, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(imprison, 9100000, 'Cause_confinement', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(imprison, 9100000, 'Cause_confinement', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(pocket, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(pocket, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(pot, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(pot, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(sheathe, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(sheathe, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(shelve, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(shelve, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(shoulder, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(shoulder, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(warehouse, 9100000, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(warehouse, 9100000, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(lodge, 9100100, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(lodge, 9100100, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(stable, 9100100, 'Placing', ['Agent': 'Agent', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(stable, 9100100, 'Placing', ['Agent': 'Cause', 'Location': 'Goal', 'Theme': 'Theme']).
fnpattern(depose, 10010000, 'Change_of_leadership', ['Agent': 'Selector', 'Theme': 'Old_leader', 'Source': 'Role']).
fnpattern(depose, 10010000, 'Change_of_leadership', ['Agent': 'Selector', 'Theme': 'Old_order', 'Source': 'Role']).
fnpattern(dislodge, 10010000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(dislodge, 10010000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(eject, 10010000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(eject, 10010000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(eliminate, 10010000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(eliminate, 10010000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(evict, 10010000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(evict, 10010000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(excise, 10010000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(excise, 10010000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(expel, 10010000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(expel, 10010000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(extract, 10010000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(extract, 10010000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(oust, 10010000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(oust, 10010000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(remove, 10010000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(remove, 10010000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(withdraw, 10010000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(withdraw, 10010000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(evacuate, 10020000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(evacuate, 10020000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(expel, 10020000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(expel, 10020000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(remove, 10020000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(remove, 10020000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(clear, 10030100, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(clear, 10030100, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(clear, 10030100, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(clear, 10030100, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(drain, 10030100, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(drain, 10030100, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(drain, 10030100, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(drain, 10030100, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(empty, 10030100, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(empty, 10030100, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(empty, 10030100, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(empty, 10030100, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(expunge, 10041000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(expunge, 10041000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(purge, 10041000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(purge, 10041000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(shave, 10041000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(shave, 10041000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(skim, 10041000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(skim, 10041000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(dust, 10041100, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(dust, 10041100, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(pluck, 10041100, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(pluck, 10041100, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(rinse, 10041100, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(rinse, 10041100, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(strip, 10041100, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(strip, 10041100, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(wash, 10041100, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(wash, 10041100, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(abduct, 10050000, 'Kidnapping', ['Agent': 'Perpetrator', 'Theme': 'Victim']).
fnpattern(confiscate, 10050000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source', 'Beneficiary': 'Goal']).
fnpattern(confiscate, 10050000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Beneficiary': 'Goal']).
fnpattern(embezzle, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Purpose']).
fnpattern(embezzle, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Reason']).
fnpattern(embezzle, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Purpose']).
fnpattern(embezzle, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Reason']).
fnpattern(filch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Purpose']).
fnpattern(filch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Reason']).
fnpattern(filch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Purpose']).
fnpattern(filch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Reason']).
fnpattern(kidnap, 10050000, 'Kidnapping', ['Agent': 'Perpetrator', 'Theme': 'Victim']).
fnpattern(lift, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Purpose']).
fnpattern(lift, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Reason']).
fnpattern(lift, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Purpose']).
fnpattern(lift, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Reason']).
fnpattern(misappropriate, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Purpose']).
fnpattern(misappropriate, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Reason']).
fnpattern(misappropriate, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Purpose']).
fnpattern(misappropriate, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Reason']).
fnpattern(pilfer, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Purpose']).
fnpattern(pilfer, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Reason']).
fnpattern(pilfer, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Purpose']).
fnpattern(pilfer, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Reason']).
fnpattern(pinch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Purpose']).
fnpattern(pinch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Reason']).
fnpattern(pinch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Purpose']).
fnpattern(pinch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Reason']).
fnpattern(pirate, 10050000, 'Piracy', ['Agent': 'Perpetrator', 'Theme': 'Vehicle']).
fnpattern(purloin, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Purpose']).
fnpattern(purloin, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Reason']).
fnpattern(purloin, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Purpose']).
fnpattern(purloin, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Reason']).
fnpattern(smuggle, 10050000, 'Smuggling', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Goal']).
fnpattern(snatch, 10050000, 'Kidnapping', ['Agent': 'Perpetrator', 'Theme': 'Victim']).
fnpattern(snatch, 10050000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source', 'Beneficiary': 'Goal']).
fnpattern(snatch, 10050000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Beneficiary': 'Goal']).
fnpattern(snatch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Purpose']).
fnpattern(snatch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Reason']).
fnpattern(snatch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Purpose']).
fnpattern(snatch, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Reason']).
fnpattern(steal, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Purpose']).
fnpattern(steal, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Reason']).
fnpattern(steal, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Purpose']).
fnpattern(steal, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Reason']).
fnpattern(swipe, 10050000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source', 'Beneficiary': 'Goal']).
fnpattern(swipe, 10050000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Beneficiary': 'Goal']).
fnpattern(swipe, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Purpose']).
fnpattern(swipe, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Reason']).
fnpattern(swipe, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Purpose']).
fnpattern(swipe, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Reason']).
fnpattern(take, 10050000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source', 'Beneficiary': 'Goal']).
fnpattern(take, 10050000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Beneficiary': 'Goal']).
fnpattern(thieve, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Purpose']).
fnpattern(thieve, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source', 'Beneficiary': 'Reason']).
fnpattern(thieve, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Purpose']).
fnpattern(thieve, 10050000, 'Theft', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim', 'Beneficiary': 'Reason']).
fnpattern(cure, 10060000, 'Cure', ['Agent': 'Healer', 'Theme': 'Affliction', 'Source': 'Patient']).
fnpattern(cure, 10060000, 'Cure', ['Agent': 'Healer', 'Theme': 'Body_part', 'Source': 'Patient']).
fnpattern(denude, 10060000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(denude, 10060000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(divest, 10060000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(divest, 10060000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(purge, 10060000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(purge, 10060000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(purge, 10060000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(purge, 10060000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(rid, 10060000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(rid, 10060000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(rob, 10060000, 'Robbery', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Victim']).
fnpattern(rob, 10060000, 'Robbery', ['Agent': 'Perpetrator', 'Theme': 'Goods', 'Source': 'Source']).
fnpattern(strip, 10060000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(strip, 10060000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(strip, 10060000, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(strip, 10060000, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(void, 10060000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(void, 10060000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(drain, 10060100, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(drain, 10060100, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(drain, 10060100, 'Removing', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(drain, 10060100, 'Removing', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(ease, 10060100, 'Cure', ['Agent': 'Healer', 'Theme': 'Affliction', 'Source': 'Patient']).
fnpattern(ease, 10060100, 'Cure', ['Agent': 'Healer', 'Theme': 'Body_part', 'Source': 'Patient']).
fnpattern(bone, 10070000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(bone, 10070000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(core, 10070000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(core, 10070000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(gut, 10070000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(gut, 10070000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(peel, 10070000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(peel, 10070000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(scalp, 10070000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(scalp, 10070000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(skin, 10070000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(skin, 10070000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(debug, 10080000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(debug, 10080000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(deforest, 10080000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(deforest, 10080000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(defrost, 10080000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(defrost, 10080000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(degrease, 10080000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(degrease, 10080000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(delouse, 10080000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(delouse, 10080000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(descale, 10080000, 'Emptying', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(descale, 10080000, 'Emptying', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(dispatch, 11010000, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Goal']).
fnpattern(dispatch, 11010000, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Recipient']).
fnpattern(post, 11010000, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Goal']).
fnpattern(post, 11010000, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Recipient']).
fnpattern(forward, 11010100, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Goal']).
fnpattern(forward, 11010100, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Recipient']).
fnpattern(mail, 11010100, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Goal']).
fnpattern(mail, 11010100, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Recipient']).
fnpattern(send, 11010100, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Goal']).
fnpattern(send, 11010100, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Recipient']).
fnpattern(ship, 11010100, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Goal']).
fnpattern(ship, 11010100, 'Sending', ['Agent': 'Sender', 'Theme': 'Theme', 'Destination': 'Recipient']).
fnpattern(drag, 11040000, 'Cause_motion', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(drag, 11040000, 'Cause_motion', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(tug, 11040000, 'Cause_motion', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(tug, 11040000, 'Cause_motion', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(push, 11040110, 'Cause_motion', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(push, 11040110, 'Cause_motion', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(shove, 11040110, 'Cause_motion', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(shove, 11040110, 'Cause_motion', ['Agent': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(pull, 12000100, 'Manipulation', ['Agent': 'Agent', 'Theme': 'Entity']).
fnpattern(tug, 12000100, 'Manipulation', ['Agent': 'Agent', 'Theme': 'Entity']).
fnpattern(yank, 12000100, 'Manipulation', ['Agent': 'Agent', 'Theme': 'Entity']).
fnpattern(push, 12000110, 'Manipulation', ['Agent': 'Agent', 'Theme': 'Entity']).
fnpattern(thrust, 12000110, 'Cause_motion', ['Agent': 'Agent', 'Theme': 'Theme']).
fnpattern(pass, 13010000, 'Giving', ['Agent': 'Donor', 'Theme': 'Theme', 'Recipient': 'Recipient']).
fnpattern(give, 13010100, 'Giving', ['Agent': 'Donor', 'Theme': 'Theme', 'Recipient': 'Recipient']).
fnpattern(lease, 13010100, 'Commerce_buy', ['Agent': 'Buyer', 'Theme': 'Goods']).
fnpattern(lease, 13010100, 'Commerce_sell', ['Agent': 'Seller', 'Theme': 'Goods']).
fnpattern(rent, 13010100, 'Commerce_buy', ['Agent': 'Buyer', 'Theme': 'Goods']).
fnpattern(rent, 13010100, 'Commerce_sell', ['Agent': 'Seller', 'Theme': 'Goods']).
fnpattern(sell, 13010100, 'Commerce_sell', ['Agent': 'Seller', 'Theme': 'Goods']).
fnpattern(pay, 13010110, 'Commerce_pay', ['Agent': 'Seller', 'Recipient': 'Buyer', 'Theme': 'Goods', 'Asset': 'Money']).
fnpattern(donate, 13020110, 'Giving', ['Agent': 'Donor', 'Theme': 'Theme', 'Recipient': 'Recipient']).
fnpattern(donate, 13020110, 'Giving', ['Cause': 'Donor', 'Theme': 'Theme', 'Recipient': 'Recipient']).
fnpattern(disburse, 13020200, 'Commerce_pay', ['Agent': 'Seller', 'Recipient': 'Buyer', 'Theme': 'Goods']).
fnpattern(disburse, 13020200, 'Commerce_pay', ['Agent': 'Cause', 'Recipient': 'Buyer', 'Theme': 'Goods']).
fnpattern(bequeath, 13030000, 'Giving', ['Agent': 'Donor', 'Theme': 'Theme', 'Recipient': 'Recipient']).
fnpattern(charge, 13042000, 'Commerce_collect', ['Agent': 'Buyer', 'Theme': 'Goods']).
fnpattern(buy, 13051000, 'Commerce_buy', ['Agent': 'Buyer', 'Theme': 'Goods']).
fnpattern(call, 13051000, 'Claim_ownership', ['Agent': 'Claimant', 'Theme': 'Property', 'Beneficiary': 'Beneficiary']).
fnpattern(hire, 13051000, 'Hiring', ['Agent': 'Employer', 'Theme': 'Employee']).
fnpattern(lease, 13051000, 'Commerce_buy', ['Agent': 'Buyer', 'Theme': 'Goods']).
fnpattern(order, 13051000, 'Request', ['Agent': 'Speaker', 'Theme': 'Message', 'Source': 'Addressee']).
fnpattern(pick, 13051000, 'Choosing', ['Agent': 'Cognizer', 'Theme': 'Chosen']).
fnpattern(rent, 13051000, 'Commerce_buy', ['Agent': 'Buyer', 'Theme': 'Goods']).
fnpattern(secure, 13051000, 'Getting', ['Agent': 'Recipient', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(gain, 13051100, 'Getting', ['Agent': 'Recipient', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(get, 13051100, 'Getting', ['Agent': 'Recipient', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(accept, 13052000, 'Receiving', ['Agent': 'Donor', 'Theme': 'Theme']).
fnpattern(collect, 13052000, 'Commerce_collect', ['Agent': 'Buyer', 'Source': 'Seller', 'Theme': 'Goods', 'Asset': 'Money']).
fnpattern(receive, 13052000, 'Receiving', ['Agent': 'Donor', 'Theme': 'Theme']).
fnpattern(seize, 13052000, 'Taking', ['Agent': 'Agent', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(select, 13052000, 'Choosing', ['Agent': 'Cognizer', 'Theme': 'Chosen']).
fnpattern(acquire, 13052100, 'Getting', ['Agent': 'Recipient', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(obtain, 13052100, 'Getting', ['Agent': 'Recipient', 'Theme': 'Theme', 'Source': 'Source']).
fnpattern(purchase, 13052100, 'Commerce_buy', ['Agent': 'Buyer', 'Theme': 'Goods']).
fnpattern(replace, 13060000, 'Replacing', ['Agent': 'Agent', 'Theme': 'Old', 'Theme2': 'New', 'Beneficiary': 'Purpose']).
fnpattern(replace, 13060000, 'Replacing', ['Agent': 'Agent', 'Theme': 'Old', 'Theme2': 'New', 'Beneficiary': 'Reason']).
fnpattern(replace, 13060000, 'Replacing', ['Agent': 'Agent', 'Theme1': 'Old', 'Theme2': 'New', 'Beneficiary': 'Purpose']).
fnpattern(replace, 13060000, 'Replacing', ['Agent': 'Agent', 'Theme1': 'Old', 'Theme2': 'New', 'Beneficiary': 'Reason']).
fnpattern(acquire, 14000000, 'Getting', ['Agent': 'Recipient', 'Topic': 'Theme', 'Source': 'Source']).
fnpattern(read, 14000100, 'Reading', ['Agent': 'Reader', 'Topic': 'Text']).
fnpattern(study, 14000100, 'Education_teaching', ['Agent': 'Student', 'Topic': 'Subject', 'Source': 'Teacher']).
fnpattern(read, 14000210, 'Reading', ['Agent': 'Reader', 'Topic': 'Text']).
fnpattern(clasp, 15010100, 'Manipulation', ['Agent': 'Agent', 'Theme': 'Entity']).
fnpattern(clutch, 15010100, 'Manipulation', ['Agent': 'Agent', 'Theme': 'Entity']).
fnpattern(grasp, 15010100, 'Manipulation', ['Agent': 'Agent', 'Theme': 'Entity']).
fnpattern(grip, 15010100, 'Manipulation', ['Agent': 'Agent', 'Theme': 'Entity']).
fnpattern(block, 16000000, 'Eclipse', ['Agent': 'Obstruction', 'Patient': 'Eclipsed']).
fnpattern(conceal, 16000000, 'Eclipse', ['Agent': 'Obstruction', 'Patient': 'Eclipsed']).
fnpattern(hide, 16000000, 'Eclipse', ['Agent': 'Obstruction', 'Patient': 'Eclipsed']).
fnpattern(screen, 16000000, 'Eclipse', ['Agent': 'Obstruction', 'Patient': 'Eclipsed']).
fnpattern(hide, 16000100, 'Eclipse', ['Agent': 'Obstruction', 'Patient': 'Eclipsed']).
fnpattern(discard, 17010000, 'Removing', ['Agent': 'Agent', 'Cause': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(chuck, 17010100, 'Cause_motion', ['Agent': 'Agent', 'Cause': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(fire, 17010100, 'Shoot_projectiles', ['Agent': 'Agent', 'Theme': 'Projectile', 'Source': 'Firearm']).
fnpattern(fling, 17010100, 'Cause_motion', ['Agent': 'Agent', 'Cause': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(hit, 17010100, 'Cause_impact', ['Agent': 'Agent', 'Theme': 'Impactor', 'Destination': 'Impactee']).
fnpattern(hit, 17010100, 'Cause_impact', ['Cause': 'Agent', 'Theme': 'Impactor', 'Destination': 'Impactee']).
fnpattern(hurl, 17010100, 'Cause_motion', ['Agent': 'Agent', 'Cause': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(nudge, 17010100, 'Cause_motion', ['Agent': 'Agent', 'Cause': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(pitch, 17010100, 'Cause_motion', ['Agent': 'Agent', 'Cause': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(shoot, 17010100, 'Shoot_projectiles', ['Agent': 'Agent', 'Theme': 'Projectile', 'Source': 'Firearm']).
fnpattern(shove, 17010100, 'Cause_motion', ['Agent': 'Agent', 'Cause': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(slam, 17010100, 'Cause_impact', ['Agent': 'Agent', 'Theme': 'Impactor', 'Destination': 'Impactee']).
fnpattern(slam, 17010100, 'Cause_impact', ['Cause': 'Agent', 'Theme': 'Impactor', 'Destination': 'Impactee']).
fnpattern(slap, 17010100, 'Cause_impact', ['Agent': 'Agent', 'Theme': 'Impactor', 'Destination': 'Impactee']).
fnpattern(slap, 17010100, 'Cause_impact', ['Cause': 'Agent', 'Theme': 'Impactor', 'Destination': 'Impactee']).
fnpattern(catapult, 17010110, 'Cause_motion', ['Agent': 'Agent', 'Cause': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(throw, 17010110, 'Cause_motion', ['Agent': 'Agent', 'Cause': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(toss, 17010110, 'Cause_motion', ['Agent': 'Agent', 'Cause': 'Cause', 'Theme': 'Theme', 'Source': 'Source', 'Destination': 'Goal']).
fnpattern(buffet, 17020000, 'Cause_harm', ['Agent': 'Agent', 'Theme': 'Victim']).
fnpattern(buffet, 17020000, 'Cause_harm', ['Agent': 'Agent', 'Theme': 'Body_part']).
fnpattern(buffet, 17020000, 'Cause_harm', ['Agent': 'Cause', 'Theme': 'Victim']).
fnpattern(buffet, 17020000, 'Cause_harm', ['Agent': 'Cause', 'Theme': 'Body_part']).
fnpattern(pelt, 17020000, 'Cause_harm', ['Agent': 'Agent', 'Theme': 'Victim']).
fnpattern(pelt, 17020000, 'Cause_harm', ['Agent': 'Agent', 'Theme': 'Body_part']).
fnpattern(pelt, 17020000, 'Cause_harm', ['Agent': 'Cause', 'Theme': 'Victim']).
fnpattern(pelt, 17020000, 'Cause_harm', ['Agent': 'Cause', 'Theme': 'Body_part']).
fnpattern(stone, 17020000, 'Cause_harm', ['Agent': 'Agent', 'Theme': 'Victim']).
fnpattern(stone, 17020000, 'Cause_harm', ['Agent': 'Agent', 'Theme': 'Body_part']).
fnpattern(stone, 17020000, 'Cause_harm', ['Agent': 'Cause', 'Theme': 'Victim']).
fnpattern(stone, 17020000, 'Cause_harm', ['Agent': 'Cause', 'Theme': 'Body_part']).
fnpattern(bang, 18010100, 'Cause_impact', ['Agent': 'Agent', 'Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(bang, 18010100, 'Impact', ['Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(bash, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(bash, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(bash, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(bash, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(batter, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(batter, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(batter, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(batter, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(beat, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(beat, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(beat, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(beat, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(bump, 18010100, 'Impact', ['Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(butt, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(butt, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(butt, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(butt, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(click, 18010100, 'Cause_impact', ['Agent': 'Agent', 'Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(click, 18010100, 'Impact', ['Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(hammer, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(hammer, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(hammer, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(hammer, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(hit, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(hit, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(hit, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(hit, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(hit, 18010100, 'Cause_impact', ['Agent': 'Agent', 'Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(hit, 18010100, 'Hit_target', ['Agent': 'Agent', 'Patient': 'Target']).
fnpattern(hit, 18010100, 'Hit_target', ['Instrument': 'Agent', 'Patient': 'Target']).
fnpattern(hit, 18010100, 'Impact', ['Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(kick, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(kick, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(kick, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(kick, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(knock, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(knock, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(knock, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(knock, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(lash, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(lash, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(lash, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(lash, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(rap, 18010100, 'Cause_impact', ['Agent': 'Agent', 'Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(rap, 18010100, 'Impact', ['Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(slap, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(slap, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(slap, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(slap, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(slap, 18010100, 'Cause_impact', ['Agent': 'Agent', 'Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(slap, 18010100, 'Impact', ['Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(smack, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(smack, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(smack, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(smack, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(smack, 18010100, 'Cause_impact', ['Agent': 'Agent', 'Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(smack, 18010100, 'Impact', ['Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(strike, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(strike, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(strike, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(strike, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(strike, 18010100, 'Cause_impact', ['Agent': 'Agent', 'Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(strike, 18010100, 'Impact', ['Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(thump, 18010100, 'Cause_impact', ['Agent': 'Agent', 'Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(thump, 18010100, 'Impact', ['Instrument': 'Impactor', 'Patient': 'Impactee']).
fnpattern(thwack, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(thwack, 18010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(thwack, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(thwack, 18010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(claw, 18020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(claw, 18020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(claw, 18020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(claw, 18020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(punch, 18020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(punch, 18020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(punch, 18020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(punch, 18020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(stab, 18020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(stab, 18020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(stab, 18020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(stab, 18020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(belt, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(belt, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(belt, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(belt, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(bludgeon, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(bludgeon, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(bludgeon, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(bludgeon, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(club, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(club, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(club, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(club, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(cudgel, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(cudgel, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(cudgel, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(cudgel, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(cuff, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(cuff, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(cuff, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(cuff, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(knife, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(knife, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(knife, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(knife, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(pummel, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(pummel, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(pummel, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(pummel, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(whip, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(whip, 18030000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(whip, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(whip, 18030000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(hit, 18040000, 'Impact', ['Theme': 'Impactor', 'Location': 'Impactee']).
fnpattern(bang, 18040100, 'Impact', ['Theme': 'Impactor', 'Location': 'Impactee']).
fnpattern(bump, 18040100, 'Impact', ['Theme': 'Impactor', 'Location': 'Impactee']).
fnpattern(crash, 18040100, 'Impact', ['Theme': 'Impactor', 'Location': 'Impactee']).
fnpattern(slam, 18040100, 'Impact', ['Theme': 'Impactor', 'Location': 'Impactee']).
fnpattern(smash, 18040100, 'Impact', ['Theme': 'Impactor', 'Location': 'Impactee']).
fnpattern(thud, 18040100, 'Impact', ['Theme': 'Impactor', 'Location': 'Impactee']).
fnpattern(jab, 19000000, 'Cause_harm', ['Agent': 'Agent', 'Destination': 'Victim']).
fnpattern(jab, 19000000, 'Cause_harm', ['Agent': 'Agent', 'Destination': 'Body_part']).
fnpattern(jab, 19000000, 'Cause_harm', ['Agent': 'Cause', 'Destination': 'Victim']).
fnpattern(jab, 19000000, 'Cause_harm', ['Agent': 'Cause', 'Destination': 'Body_part']).
fnpattern(graze, 20000100, 'Cause_impact', ['Agent': 'Agent', 'Instrument': 'Impactor', 'Experiencer': 'Impactee']).
fnpattern(graze, 20000100, 'Impact', ['Instrument': 'Impactor', 'Experiencer': 'Impactee']).
fnpattern(chip, 21010100, 'Damaging', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(cut, 21010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(cut, 21010100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(cut, 21010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(cut, 21010100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(cut, 21010100, 'Experience_bodily_harm', ['Agent': 'Agent', 'Patient': 'Patient', 'Instrument': 'Injuring_entity']).
fnpattern(scratch, 21010100, 'Damaging', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(bruise, 21020100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(bruise, 21020100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(bruise, 21020100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(bruise, 21020100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(chip, 21020100, 'Damaging', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(crush, 21020100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(crush, 21020100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(crush, 21020100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(crush, 21020100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(crush, 21020100, 'Grinding', ['Agent': 'Grinder', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(dent, 21020100, 'Damaging', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(gash, 21020100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(gash, 21020100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(gash, 21020100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(gash, 21020100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(grate, 21020100, 'Grinding', ['Agent': 'Grinder', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(grind, 21020100, 'Grinding', ['Agent': 'Grinder', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(nick, 21020100, 'Damaging', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(pulverize, 21020100, 'Grinding', ['Agent': 'Grinder', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(shred, 21020100, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient': 'Whole_patient']).
fnpattern(shred, 21020100, 'Grinding', ['Agent': 'Grinder', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(slice, 21020100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(slice, 21020100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(slice, 21020100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(slice, 21020100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(squash, 21020100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(squash, 21020100, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(squash, 21020100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(squash, 21020100, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(chop, 21020200, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(chop, 21020200, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(chop, 21020200, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(chop, 21020200, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(mill, 21020200, 'Grinding', ['Agent': 'Grinder', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(punch, 21020200, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(punch, 21020200, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(punch, 21020200, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(punch, 21020200, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(spear, 21020200, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(spear, 21020200, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(spear, 21020200, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(spear, 21020200, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(blend, 22010110, 'Amalgamation', ['Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(blend, 22010110, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(combine, 22010110, 'Amalgamation', ['Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(combine, 22010110, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(commingle, 22010110, 'Amalgamation', ['Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(commingle, 22010110, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(fuse, 22010110, 'Amalgamation', ['Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(fuse, 22010110, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(merge, 22010110, 'Amalgamation', ['Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(merge, 22010110, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(mix, 22010110, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(connect, 22010210, 'Make_cognitive_connection', ['Agent': 'Cognizer', 'Patient1': 'Concept_1', 'Patient2': 'Concept_2']).
fnpattern(join, 22010210, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(link, 22010210, 'Make_cognitive_connection', ['Agent': 'Cognizer', 'Patient1': 'Concept_1', 'Patient2': 'Concept_2']).
fnpattern(match, 22020100, 'Compatibility', ['Patient1': 'Item_1', 'Patient2': 'Item_2', 'Patient': 'Items']).
fnpattern(amalgamate, 22020110, 'Amalgamation', ['Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(amalgamate, 22020110, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(harmonize, 22020210, 'Compatibility', ['Patient1': 'Item_1', 'Patient2': 'Item_2', 'Patient': 'Items']).
fnpattern(rhyme, 22020210, 'Compatibility', ['Patient1': 'Item_1', 'Patient2': 'Item_2', 'Patient': 'Items']).
fnpattern(unify, 22020210, 'Amalgamation', ['Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(unify, 22020210, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(unite, 22020210, 'Amalgamation', ['Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(unite, 22020210, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(lump, 22030100, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(fuse, 22030110, 'Amalgamation', ['Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(fuse, 22030110, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(sew, 22030200, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(stick, 22030200, 'Inchoative_attaching', ['Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(attach, 22030210, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(attach, 22030210, 'Inchoative_attaching', ['Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(bind, 22030210, 'Inchoative_attaching', ['Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(bond, 22030210, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(fasten, 22030210, 'Inchoative_attaching', ['Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(moor, 22030210, 'Inchoative_attaching', ['Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(weld, 22030210, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(anchor, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(cement, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(chain, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(fetter, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(glue, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(handcuff, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(hitch, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(hook, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(lash, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(manacle, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(moor, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(nail, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(pin, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(plaster, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(rivet, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(shackle, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(solder, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(staple, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(tack, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(tape, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(tether, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(tie, 22040000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Connector', 'Patient2': 'Goal']).
fnpattern(divide, 23010100, 'Separation', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(segregate, 23010100, 'Separation', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(part, 23010200, 'Separation', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(separate, 23010200, 'Separation', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(break, 23020000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient1': 'Whole_patient']).
fnpattern(rip, 23020000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient1': 'Whole_patient']).
fnpattern(split, 23020000, 'Separation', ['Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(tear, 23020000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient1': 'Whole_patient']).
fnpattern(detach, 23030000, 'Attaching', ['Agent': 'Agent', 'Patient1': 'Item', 'Patient2': 'Goal']).
fnpattern(dismantle, 23030000, 'Destroying', ['Agent': 'Agent', 'Patient': 'Undergoer']).
fnpattern(dismantle, 23030000, 'Destroying', ['Agent': 'Destroyer', 'Patient': 'Undergoer']).
fnpattern(partition, 23030000, 'Separation', ['Agent': 'Agent', 'Patient1': 'Part_1', 'Patient2': 'Part_2']).
fnpattern(unbuckle, 23030000, 'Closure', ['Agent': 'Agent', 'Patient1': 'Fastener', 'Patient2': 'Containing_object']).
fnpattern(unfasten, 23030000, 'Closure', ['Agent': 'Agent', 'Patient1': 'Fastener', 'Patient2': 'Containing_object']).
fnpattern(unscrew, 23030000, 'Closure', ['Agent': 'Agent', 'Patient1': 'Fastener', 'Patient2': 'Containing_object']).
fnpattern(unzip, 23030000, 'Closure', ['Agent': 'Agent', 'Patient1': 'Fastener', 'Patient2': 'Containing_object']).
fnpattern(dye, 24000000, 'Processing_materials', ['Agent': 'Agent', 'Theme': 'Material']).
fnpattern(glaze, 24000000, 'Filling', ['Agent': 'Agent', 'Theme': 'Theme']).
fnpattern(paint, 24000000, 'Filling', ['Agent': 'Agent', 'Theme': 'Theme']).
fnpattern(stain, 24000000, 'Processing_materials', ['Agent': 'Agent', 'Theme': 'Material']).
fnpattern(varnish, 24000000, 'Filling', ['Agent': 'Agent', 'Theme': 'Theme']).
fnpattern(etch, 25010000, 'Processing_materials', ['Agent': 'Agent', 'Destination': 'Material']).
fnpattern(copy, 25020000, 'Duplication', ['Theme': 'Copy', 'Agent': 'Creator']).
fnpattern(forge, 25020000, 'Imitation', ['Theme': 'Text', 'Agent': 'Author']).
fnpattern(misspell, 25020000, 'Spelling_and_pronouncing', ['Theme': 'Formal_realization', 'Agent': 'Speaker', 'Destination': 'Context']).
fnpattern(print, 25020000, 'Text_creation', ['Theme': 'Text', 'Agent': 'Author']).
fnpattern(spell, 25020000, 'Spelling_and_pronouncing', ['Theme': 'Formal_realization', 'Agent': 'Speaker', 'Destination': 'Context']).
fnpattern(type, 25020000, 'Text_creation', ['Theme': 'Text', 'Agent': 'Author']).
fnpattern(write, 25020000, 'Text_creation', ['Theme': 'Text', 'Agent': 'Author']).
fnpattern(gild, 25030000, 'Filling', ['Agent': 'Agent', 'Destination': 'Goal', 'Theme': 'Theme']).
fnpattern(chronicle, 25040000, 'Text_creation', ['Theme': 'Text', 'Agent': 'Author', 'Destination': 'Addressee']).
fnpattern(chronicle, 25040000, 'Text_creation', ['Theme': 'Text', 'Agent': 'Author', 'Destination': 'Place']).
fnpattern(copy, 25040000, 'Duplication', ['Theme': 'Copy', 'Agent': 'Creator']).
fnpattern(forge, 25040000, 'Imitation', ['Theme': 'Copy', 'Agent': 'Creator']).
fnpattern(photocopy, 25040000, 'Duplication', ['Theme': 'Copy', 'Agent': 'Creator']).
fnpattern(type, 25040000, 'Text_creation', ['Theme': 'Text', 'Agent': 'Author', 'Destination': 'Addressee']).
fnpattern(type, 25040000, 'Text_creation', ['Theme': 'Text', 'Agent': 'Author', 'Destination': 'Place']).
fnpattern(assemble, 26010000, 'Building', ['Material': 'Components', 'Product': 'Created_entity', 'Agent': 'Agent']).
fnpattern(bake, 26010000, 'Cooking_creation', ['Product': 'Produced_food', 'Agent': 'Cook']).
fnpattern(cook, 26010000, 'Cooking_creation', ['Product': 'Produced_food', 'Agent': 'Cook']).
fnpattern(cook_up, 26010000, 'Invention', ['Product': 'Invention', 'Agent': 'Cognizer']).
fnpattern(formulate, 26010000, 'Invention', ['Product': 'Invention', 'Agent': 'Cognizer']).
fnpattern(hatch, 26010000, 'Invention', ['Product': 'Invention', 'Agent': 'Cognizer']).
fnpattern(build, 26010100, 'Building', ['Material': 'Components', 'Product': 'Created_entity', 'Agent': 'Agent']).
fnpattern(make, 26010100, 'Building', ['Material': 'Components', 'Product': 'Created_entity', 'Agent': 'Agent']).
fnpattern(make, 26010100, 'Cooking_creation', ['Product': 'Produced_food', 'Agent': 'Cook']).
fnpattern(make, 26010100, 'Intentionally_create', ['Product': 'Created_entity', 'Agent': 'Creator']).
fnpattern(make, 26010100, 'Manufacturing', ['Product': 'Product', 'Agent': 'Manufacturer']).
fnpattern(develop, 26020000, 'Coming_to_be', ['Product': 'Entity', 'Material': 'Components']).
fnpattern(hatch, 26020000, 'Invention', ['Product': 'Invention', 'Agent': 'Cognizer']).
fnpattern(bake, 26030100, 'Cooking_creation', ['Product': 'Produced_food', 'Agent': 'Cook']).
fnpattern(cook, 26030100, 'Cooking_creation', ['Product': 'Produced_food', 'Agent': 'Cook']).
fnpattern(light, 26030100, 'Setting_fire', ['Product': 'Flamables', 'Agent': 'Kindler']).
fnpattern(prepare, 26030100, 'Cooking_creation', ['Product': 'Produced_food', 'Agent': 'Cook']).
fnpattern(coin, 26040000, 'Achieving_first', ['Product': 'New_idea', 'Agent': 'Cognizer']).
fnpattern(coin, 26040000, 'Invention', ['Product': 'Invention', 'Agent': 'Cognizer']).
fnpattern(compose, 26040000, 'Text_creation', ['Product': 'Text', 'Agent': 'Author']).
fnpattern(concoct, 26040000, 'Cooking_creation', ['Product': 'Produced_food', 'Material': 'Ingredients', 'Agent': 'Cook']).
fnpattern(concoct, 26040000, 'Invention', ['Product': 'Invention', 'Agent': 'Cognizer']).
fnpattern(construct, 26040000, 'Building', ['Product': 'Created_entity', 'Material': 'Components', 'Agent': 'Agent']).
fnpattern(contrive, 26040000, 'Invention', ['Product': 'Invention', 'Agent': 'Cognizer']).
fnpattern(create, 26040000, 'Intentionally_create', ['Product': 'Created_entity', 'Agent': 'Creator']).
fnpattern(invent, 26040000, 'Achieving_first', ['Product': 'New_idea', 'Agent': 'Cognizer']).
fnpattern(invent, 26040000, 'Invention', ['Product': 'Invention', 'Agent': 'Cognizer']).
fnpattern(manufacture, 26040000, 'Manufacturing', ['Product': 'Product', 'Material': 'Resource', 'Agent': 'Manufacturer']).
fnpattern(produce, 26040000, 'Manufacturing', ['Product': 'Product', 'Material': 'Resource', 'Agent': 'Manufacturer']).
fnpattern(design, 26040100, 'Invention', ['Product': 'Invention', 'Agent': 'Cognizer']).
fnpattern(stage, 26040100, 'Behind_the_scenes', ['Product': 'Production', 'Material': 'Medium', 'Agent': 'Artist']).
fnpattern(bend, 26050000, 'Reshaping', ['Product': 'Result', 'Material': 'Undergoer', 'Agent': 'Deformer']).
fnpattern(fold, 26050000, 'Reshaping', ['Product': 'Result', 'Material': 'Undergoer', 'Agent': 'Deformer']).
fnpattern(knead, 26050000, 'Manipulation', ['Agent': 'Agent', 'Product': 'Entity']).
fnpattern(squash, 26050000, 'Reshaping', ['Product': 'Result', 'Material': 'Undergoer', 'Agent': 'Deformer']).
fnpattern(squeeze, 26050000, 'Manipulation', ['Agent': 'Agent', 'Product': 'Entity']).
fnpattern(squish, 26050000, 'Reshaping', ['Product': 'Result', 'Material': 'Undergoer', 'Agent': 'Deformer']).
fnpattern(deform, 26061000, 'Reshaping', ['Agent': 'Deformer', 'Material': 'Undergoer', 'Product': 'Result']).
fnpattern(translate, 26061000, 'Categorization', ['Agent': 'Cognizer', 'Patient': 'Item', 'Product': 'Category']).
fnpattern(switch, 26062100, 'Replacing', ['Agent': 'Agent', 'Source': 'Old', 'Destination': 'New']).
fnpattern(direct, 26070100, 'Behind_the_scenes', ['Agent': 'Artist', 'Theme': 'Production', 'Beneficiary': 'Studio']).
fnpattern(improvise, 26070100, 'Invention', ['Agent': 'Cognizer', 'Theme': 'Invention']).
fnpattern(play, 26070110, 'Performers_and_roles', ['Agent': 'Performer', 'Theme': 'Performance', 'Beneficiary': 'Audience']).
fnpattern(compose, 26070200, 'Text_creation', ['Agent': 'Author', 'Theme': 'Text', 'Beneficiary': 'Purpose']).
fnpattern(produce, 26070200, 'Behind_the_scenes', ['Agent': 'Artist', 'Theme': 'Production', 'Beneficiary': 'Studio']).
fnpattern(write, 26070210, 'Text_creation', ['Agent': 'Author', 'Theme': 'Text', 'Beneficiary': 'Purpose']).
fnpattern(cause, 27000000, 'Causation', ['Theme1': 'Cause', 'Theme2': 'Effect']).
fnpattern(create, 27000000, 'Cause_to_start', ['Theme1': 'Cause', 'Theme2': 'Effect']).
fnpattern(engender, 27000000, 'Cause_to_start', ['Theme1': 'Cause', 'Theme2': 'Effect']).
fnpattern(generate, 27000000, 'Cause_to_start', ['Theme1': 'Cause', 'Theme2': 'Effect']).
fnpattern(sire, 27000000, 'Birth', ['Theme1': 'Mother', 'Theme2': 'Child']).
fnpattern(calve, 28000000, 'Birth', ['Agent': 'Parents', 'Patient': 'Child']).
fnpattern(spawn, 28000000, 'Birth', ['Agent': 'Parents', 'Patient': 'Child']).
fnpattern(whelp, 28000000, 'Birth', ['Agent': 'Parents', 'Patient': 'Child']).
fnpattern(appoint, 29010100, 'Change_of_leadership', ['Agent': 'Selector', 'Theme': 'New_leader', 'Predicate': 'Role']).
fnpattern(elect, 29010100, 'Change_of_leadership', ['Agent': 'Selector', 'Theme': 'New_leader', 'Predicate': 'Role']).
fnpattern(name, 29010100, 'Name_conferral', ['Agent': 'Agent', 'Theme': 'Entity', 'Predicate': 'Name']).
fnpattern(imagine, 29010200, 'Awareness', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(reckon, 29010200, 'Awareness', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(depict, 29020000, 'Communicate_categorization', ['Agent': 'Speaker', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(cite, 29020100, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(class, 29020100, 'Categorization', ['Agent': 'Cognizer', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(classify, 29020100, 'Categorization', ['Agent': 'Cognizer', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(count, 29020100, 'Categorization', ['Agent': 'Cognizer', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(address, 29020200, 'Speak_on_topic', ['Agent': 'Speaker', 'Theme': 'Topic']).
fnpattern(address, 29020200, 'Topic', ['Agent': 'Communicator', 'Theme': 'Topic']).
fnpattern(appreciate, 29020200, 'Judgment', ['Agent': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(characterize, 29020200, 'Communicate_categorization', ['Agent': 'Speaker', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(know, 29020200, 'Awareness', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(perceive, 29020200, 'Categorization', ['Agent': 'Cognizer', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(recollect, 29020200, 'Memory', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(regard, 29020200, 'Categorization', ['Agent': 'Cognizer', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(remember, 29020200, 'Memory', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(treat, 29020200, 'Communicate_categorization', ['Agent': 'Speaker', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(treat, 29020200, 'Topic', ['Agent': 'Communicator', 'Theme': 'Topic']).
fnpattern(value, 29020200, 'Judgment', ['Agent': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(portray, 29020300, 'Communicate_categorization', ['Agent': 'Speaker', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(praise, 29020300, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(represent, 29020300, 'Communicate_categorization', ['Agent': 'Speaker', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(stigmatize, 29020300, 'Judgment', ['Agent': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(characterize, 29020400, 'Communicate_categorization', ['Agent': 'Speaker', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(define, 29020400, 'Communicate_categorization', ['Agent': 'Speaker', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(describe, 29020400, 'Communicate_categorization', ['Agent': 'Speaker', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(dub, 29030000, 'Name_conferral', ['Agent': 'Speaker', 'Theme': 'Entity', 'Predicate': 'Name']).
fnpattern(baptize, 29030100, 'Name_conferral', ['Agent': 'Speaker', 'Theme': 'Entity', 'Predicate': 'Name']).
fnpattern(christen, 29030100, 'Name_conferral', ['Agent': 'Speaker', 'Theme': 'Entity', 'Predicate': 'Name']).
fnpattern(name, 29030100, 'Name_conferral', ['Agent': 'Speaker', 'Theme': 'Entity', 'Predicate': 'Name']).
fnpattern(nickname, 29030100, 'Name_conferral', ['Agent': 'Speaker', 'Theme': 'Entity', 'Predicate': 'Name']).
fnpattern(avow, 29040100, 'Statement', ['Agent': 'Speaker', 'Theme': 'Addressee', 'Predicate': 'Message']).
fnpattern(confess, 29040100, 'Statement', ['Agent': 'Speaker', 'Theme': 'Addressee', 'Predicate': 'Message']).
fnpattern(declare, 29040100, 'Statement', ['Agent': 'Speaker', 'Theme': 'Addressee', 'Predicate': 'Message']).
fnpattern(profess, 29040100, 'Statement', ['Agent': 'Speaker', 'Theme': 'Addressee', 'Predicate': 'Message']).
fnpattern(believe, 29040200, 'Awareness', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(find, 29040200, 'Becoming_aware', ['Agent': 'Cognizer', 'Theme': 'Phenomenon', 'Predicate': 'State']).
fnpattern(presume, 29040200, 'Awareness', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(think, 29040200, 'Awareness', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(conjecture, 29050100, 'Statement', ['Agent': 'Speaker', 'Theme': 'Addressee', 'Predicate': 'Message']).
fnpattern(doubt, 29050100, 'Certainty', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(foresee, 29050100, 'Expectation', ['Agent': 'Cognizer', 'Theme': 'Phenomenon']).
fnpattern(foresee, 29050100, 'Expectation', ['Agent': 'Cognizer', 'Theme': 'Topic']).
fnpattern(foretell, 29050100, 'Predicting', ['Agent': 'Speaker', 'Theme': 'Eventuality']).
fnpattern(guess, 29050100, 'Coming_to_believe', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(know, 29050100, 'Awareness', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(prophesy, 29050100, 'Predicting', ['Agent': 'Speaker', 'Theme': 'Eventuality']).
fnpattern(realize, 29050100, 'Coming_to_believe', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(recognize, 29050100, 'Becoming_aware', ['Agent': 'Cognizer', 'Theme': 'Phenomenon', 'Predicate': 'State']).
fnpattern(surmise, 29050100, 'Coming_to_believe', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(suspect, 29050100, 'Awareness', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(vaticinate, 29050100, 'Predicting', ['Agent': 'Speaker', 'Theme': 'Eventuality']).
fnpattern(admit, 29050200, 'Statement', ['Agent': 'Speaker', 'Theme': 'Addressee', 'Predicate': 'Message']).
fnpattern(assert, 29050200, 'Statement', ['Agent': 'Speaker', 'Theme': 'Addressee', 'Predicate': 'Message']).
fnpattern(discover, 29050200, 'Becoming_aware', ['Agent': 'Cognizer', 'Theme': 'Phenomenon', 'Predicate': 'State']).
fnpattern(maintain, 29050200, 'Statement', ['Agent': 'Speaker', 'Theme': 'Addressee', 'Predicate': 'Message']).
fnpattern(rate, 29060000, 'Assessing', ['Agent': 'Assessor']).
fnpattern(volunteer, 29080000, 'Commitment', ['Agent': 'Speaker', 'Theme': 'Addressee']).
fnpattern(bully, 29080100, 'Manipulate_into_doing', ['Agent': 'Manipulator', 'Theme': 'Victim']).
fnpattern(butcher, 29080100, 'Killing', ['Agent': 'Killer', 'Theme': 'Victim']).
fnpattern(clerk, 29080100, 'Being_employed', ['Agent': 'Employee', 'Theme': 'Task']).
fnpattern(escort, 29080100, 'Cotheme', ['Agent': 'Source', 'Theme': 'New_leader']).
fnpattern(host, 29080100, 'Social_event', ['Agent': 'Host', 'Theme': 'Social_event']).
fnpattern(partner, 29080100, 'Collaboration', ['Agent': 'Partners', 'Theme': 'Undertaking']).
fnpattern(pioneer, 29080100, 'Achieving_first', ['Agent': 'Cognizer', 'Theme': 'New_idea']).
fnpattern(shepherd, 29080100, 'Cotheme', ['Agent': 'Source', 'Theme': 'New_leader']).
fnpattern(tutor, 29080100, 'Education_teaching', ['Agent': 'Teacher', 'Theme': 'Student']).
fnpattern(usher, 29080100, 'Cotheme', ['Agent': 'Source', 'Theme': 'New_leader']).
fnpattern(volunteer, 29080100, 'Commitment', ['Agent': 'Speaker', 'Theme': 'Addressee']).
fnpattern(star, 29080110, 'Performers_and_roles', ['Agent': 'Performer', 'Theme': 'Audience']).
fnpattern(consider, 29090111, 'Categorization', ['Agent': 'Cognizer', 'Theme': 'Item', 'Predicate': 'Category']).
fnpattern(consider, 29090111, 'Cogitation', ['Agent': 'Cognizer', 'Theme': 'Topic']).
fnpattern(detect, 30010000, 'Perception_experience', ['Experiencer': 'Perceiver', 'Stimulus': 'Phenomenon']).
fnpattern(discern, 30010000, 'Becoming_aware', ['Experiencer': 'Cognizer', 'Stimulus': 'Phenomenon']).
fnpattern(feel, 30010000, 'Perception_experience', ['Experiencer': 'Perceiver', 'Stimulus': 'Phenomenon']).
fnpattern(notice, 30010000, 'Becoming_aware', ['Experiencer': 'Cognizer', 'Stimulus': 'Phenomenon']).
fnpattern(see, 30010000, 'Perception_experience', ['Experiencer': 'Perceiver', 'Stimulus': 'Phenomenon']).
fnpattern(sense, 30010000, 'Perception_experience', ['Experiencer': 'Perceiver', 'Stimulus': 'Phenomenon']).
fnpattern(smell, 30010000, 'Perception_experience', ['Experiencer': 'Perceiver', 'Stimulus': 'Phenomenon']).
fnpattern(taste, 30010000, 'Perception_experience', ['Experiencer': 'Perceiver', 'Stimulus': 'Phenomenon']).
fnpattern(hear, 30010100, 'Perception_experience', ['Experiencer': 'Perceiver', 'Stimulus': 'Phenomenon']).
fnpattern(descry, 30020000, 'Becoming_aware', ['Experiencer': 'Cognizer', 'Stimulus': 'Phenomenon']).
fnpattern(discover, 30020000, 'Becoming_aware', ['Experiencer': 'Cognizer', 'Stimulus': 'Phenomenon']).
fnpattern(espy, 30020000, 'Becoming_aware', ['Experiencer': 'Cognizer', 'Stimulus': 'Phenomenon']).
fnpattern(experience, 30020000, 'Feeling', ['Experiencer': 'Experiencer', 'Stimulus': 'Emotion']).
fnpattern(experience, 30020000, 'Feeling', ['Experiencer': 'Experiencer', 'Stimulus': 'Emotional_state']).
fnpattern(note, 30020000, 'Becoming_aware', ['Experiencer': 'Cognizer', 'Stimulus': 'Phenomenon']).
fnpattern(observe, 30020000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(overhear, 30020000, 'Perception_experience', ['Experiencer': 'Perceiver_passive', 'Stimulus': 'Phenomenon']).
fnpattern(perceive, 30020000, 'Perception_experience', ['Experiencer': 'Perceiver_passive', 'Stimulus': 'Phenomenon']).
fnpattern(peruse, 30020000, 'Reading', ['Experiencer': 'Reader', 'Stimulus': 'Text']).
fnpattern(peruse, 30020000, 'Scrutiny', ['Experiencer': 'Cognizer', 'Stimulus': 'Phenomenon']).
fnpattern(recognize, 30020000, 'Becoming_aware', ['Experiencer': 'Cognizer', 'Stimulus': 'Phenomenon']).
fnpattern(sniff, 30020000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(spot, 30020000, 'Becoming_aware', ['Experiencer': 'Cognizer', 'Stimulus': 'Phenomenon']).
fnpattern(spy, 30020000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(view, 30020000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(watch, 30020000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(gaze, 30030000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(glance, 30030000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(listen, 30030000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(look, 30030000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(peek, 30030000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(peep, 30030000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(peer, 30030000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(sniff, 30030000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(squint, 30030000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(stare, 30030000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(feel, 30040000, 'Appearance', ['Experiencer': 'Perceiver_passive', 'Stimulus': 'Phenomenon']).
fnpattern(feel, 30040000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(look, 30040000, 'Appearance', ['Experiencer': 'Perceiver_passive', 'Stimulus': 'Phenomenon']).
fnpattern(look, 30040000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(smell, 30040000, 'Appearance', ['Experiencer': 'Perceiver_passive', 'Stimulus': 'Phenomenon']).
fnpattern(smell, 30040000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(sound, 30040000, 'Appearance', ['Experiencer': 'Perceiver_passive', 'Stimulus': 'Phenomenon']).
fnpattern(taste, 30040000, 'Appearance', ['Experiencer': 'Perceiver_passive', 'Stimulus': 'Phenomenon']).
fnpattern(taste, 30040000, 'Perception_active', ['Experiencer': 'Perceiver_agentive', 'Stimulus': 'Phenomenon']).
fnpattern(abash, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(aggravate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(aggrieve, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(alarm, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(amaze, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(anger, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(annoy, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(antagonize, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(astonish, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(astound, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(baffle, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(beguile, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(bewilder, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(bewitch, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(bore, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(calm, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(captivate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(charm, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(cheer, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(comfort, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(confuse, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(console, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(dazzle, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(delight, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(depress, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(disappoint, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(discomfit, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(disconcert, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(discourage, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(dishearten, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(displease, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(distress, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(disturb, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(embarrass, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(enchant, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(enrage, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(entertain, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(enthrall, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(exasperate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(excite, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(exhilarate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(fascinate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(faze, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(flabbergast, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(floor, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(fluster, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(frighten, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(frustrate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(gall, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(gladden, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(gratify, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(grieve, 31010000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Cause': 'Content']).
fnpattern(hearten, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(humiliate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(impress, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(incense, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(infuriate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(interest, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(intimidate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(intrigue, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(irk, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(irritate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(madden, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(mollify, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(mortify, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(mystify, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(nettle, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(offend, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(pacify, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(perplex, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(perturb, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(placate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(puzzle, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(rankle, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(reassure, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(repel, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(revolt, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(rile, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(sadden, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(satisfy, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(scare, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(shake, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(shame, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(shock, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(sicken, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(sober, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(solace, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(soothe, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(spook, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(startle, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(stimulate, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(sting, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(stir, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(stun, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(stupefy, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(surprise, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(terrify, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(thrill, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(tickle, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(torment, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(torture, 31010000, 'Cause_harm', ['Experiencer': 'Victim', 'Cause': 'Agent']).
fnpattern(torture, 31010000, 'Cause_harm', ['Experiencer': 'Victim', 'Cause': 'Cause']).
fnpattern(trouble, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(unnerve, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(unsettle, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(vex, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(wow, 31010000, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(abhor, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(admire, 31020000, 'Judgment', ['Experiencer': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(adore, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(affirm, 31020000, 'Statement', ['Experiencer': 'Speaker', 'Theme': 'Message', 'Predicate': 'Topic']).
fnpattern(affirm, 31020000, 'Statement', ['Experiencer': 'Speaker', 'Theme': 'Message', 'Predicate': 'Medium']).
fnpattern(appreciate, 31020000, 'Judgment', ['Experiencer': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(deify, 31020000, 'Judgment', ['Experiencer': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(deplore, 31020000, 'Judgment', ['Experiencer': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(despise, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(detest, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(disdain, 31020000, 'Judgment', ['Experiencer': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(dislike, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(dread, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(enjoy, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(envy, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(esteem, 31020000, 'Judgment', ['Experiencer': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(exalt, 31020000, 'Judgment', ['Experiencer': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(execrate, 31020000, 'Judgment_communication', ['Experiencer': 'Communicator', 'Theme': 'Evaluee']).
fnpattern(fancy, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(fear, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(hate, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(like, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(loathe, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(love, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(mourn, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(pity, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(prefer, 31020000, 'Partiality', ['Experiencer': 'Judge', 'Theme': 'Dispute', 'Predicate': 'Side_2']).
fnpattern(prefer, 31020000, 'Partiality', ['Experiencer': 'Judge', 'Theme': 'Dispute', 'Predicate': 'Manifestation_of_bias']).
fnpattern(prefer, 31020000, 'Partiality', ['Experiencer': 'Judge', 'Theme': 'Sides', 'Predicate': 'Side_2']).
fnpattern(prefer, 31020000, 'Partiality', ['Experiencer': 'Judge', 'Theme': 'Sides', 'Predicate': 'Manifestation_of_bias']).
fnpattern(prefer, 31020000, 'Partiality', ['Experiencer': 'Judge', 'Theme': 'Side_1', 'Predicate': 'Side_2']).
fnpattern(prefer, 31020000, 'Partiality', ['Experiencer': 'Judge', 'Theme': 'Side_1', 'Predicate': 'Manifestation_of_bias']).
fnpattern(prefer, 31020000, 'Preference', ['Experiencer': 'Experiencer', 'Theme': 'Event', 'Predicate': 'Location_of_event']).
fnpattern(prefer, 31020000, 'Preference', ['Experiencer': 'Experiencer', 'Theme': 'Event', 'Predicate': 'Contrast']).
fnpattern(prefer, 31020000, 'Preference', ['Experiencer': 'Experiencer', 'Theme': 'Focal_participant', 'Predicate': 'Location_of_event']).
fnpattern(prefer, 31020000, 'Preference', ['Experiencer': 'Experiencer', 'Theme': 'Focal_participant', 'Predicate': 'Contrast']).
fnpattern(prize, 31020000, 'Judgment', ['Experiencer': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(reaffirm, 31020000, 'Statement', ['Experiencer': 'Speaker', 'Theme': 'Message', 'Predicate': 'Topic']).
fnpattern(reaffirm, 31020000, 'Statement', ['Experiencer': 'Speaker', 'Theme': 'Message', 'Predicate': 'Medium']).
fnpattern(regret, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(relish, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(resent, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(respect, 31020000, 'Judgment', ['Experiencer': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(revere, 31020000, 'Judgment', ['Experiencer': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(rue, 31020000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Theme': 'Content']).
fnpattern(value, 31020000, 'Judgment', ['Experiencer': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Role']).
fnpattern(cheer, 31030100, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(gladden, 31030100, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(madden, 31030100, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(sicken, 31030100, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(thrill, 31030100, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(fear, 31030300, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Cause': 'Content']).
fnpattern(grieve, 31030300, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Cause': 'Content']).
fnpattern(mourn, 31030300, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Cause': 'Content']).
fnpattern(delight, 31030500, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(delight, 31030500, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Cause': 'Content']).
fnpattern(luxuriate, 31030500, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Cause': 'Content']).
fnpattern(despair, 31030600, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Cause': 'Content']).
fnpattern(disapprove, 31030600, 'Judgment', ['Experiencer': 'Cognizer', 'Cause': 'Evaluee']).
fnpattern(sicken, 31030600, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(anger, 31030800, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(delight, 31030800, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(delight, 31030800, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Cause': 'Content']).
fnpattern(grieve, 31030800, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Cause': 'Content']).
fnpattern(meditate, 31030800, 'Cogitation', ['Experiencer': 'Cognizer', 'Cause': 'Topic']).
fnpattern(mourn, 31030800, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Cause': 'Content']).
fnpattern(muse, 31030800, 'Cogitation', ['Experiencer': 'Cognizer', 'Cause': 'Topic']).
fnpattern(puzzle, 31030800, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(reflect, 31030800, 'Cogitation', ['Experiencer': 'Cognizer', 'Cause': 'Topic']).
fnpattern(ruminate, 31030800, 'Cogitation', ['Experiencer': 'Cognizer', 'Cause': 'Topic']).
fnpattern(thrill, 31030900, 'Experiencer_obj', ['Experiencer': 'Experiencer', 'Cause': 'Stimulus']).
fnpattern(covet, 32010000, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(covet, 32010000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Event': 'Content', 'Attribute': 'Reason']).
fnpattern(crave, 32010000, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(crave, 32010000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Event': 'Content', 'Attribute': 'Reason']).
fnpattern(desire, 32010000, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(desire, 32010000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Event': 'Content', 'Attribute': 'Reason']).
fnpattern(fancy, 32010000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Event': 'Content', 'Attribute': 'Reason']).
fnpattern(want, 32010000, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(want, 32010000, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Event': 'Content', 'Attribute': 'Reason']).
fnpattern(prefer, 32010100, 'Partiality', ['Experiencer': 'Judge', 'Theme': 'Dispute', 'Attribute': 'Side_2', 'Predicate': 'Manifestation_of_bias']).
fnpattern(prefer, 32010100, 'Partiality', ['Experiencer': 'Judge', 'Theme': 'Sides', 'Attribute': 'Side_2', 'Predicate': 'Manifestation_of_bias']).
fnpattern(prefer, 32010100, 'Partiality', ['Experiencer': 'Judge', 'Theme': 'Side_1', 'Attribute': 'Side_2', 'Predicate': 'Manifestation_of_bias']).
fnpattern(prefer, 32010100, 'Preference', ['Experiencer': 'Experiencer', 'Theme': 'Event', 'Predicate': 'Location_of_event']).
fnpattern(prefer, 32010100, 'Preference', ['Experiencer': 'Experiencer', 'Theme': 'Event', 'Predicate': 'Contrast']).
fnpattern(prefer, 32010100, 'Preference', ['Experiencer': 'Experiencer', 'Theme': 'Focal_participant', 'Predicate': 'Location_of_event']).
fnpattern(prefer, 32010100, 'Preference', ['Experiencer': 'Experiencer', 'Theme': 'Focal_participant', 'Predicate': 'Contrast']).
fnpattern(crave, 32020100, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(crave, 32020100, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Event': 'Content']).
fnpattern(hanker, 32020100, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(hanker, 32020100, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Event': 'Content']).
fnpattern(hope, 32020100, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(hunger, 32020100, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(long, 32020100, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(long, 32020100, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Event': 'Content']).
fnpattern(lust, 32020100, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(pine, 32020100, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Event': 'Content']).
fnpattern(thirst, 32020100, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(wish, 32020100, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(yearn, 32020100, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(yearn, 32020100, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Event': 'Content']).
fnpattern(hanker, 32020200, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(lust, 32020200, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(thirst, 32020200, 'Desiring', ['Experiencer': 'Experiencer', 'Event': 'Theme']).
fnpattern(yearn, 32020200, 'Experiencer_subj', ['Experiencer': 'Experiencer', 'Event': 'Content']).
fnpattern(acclaim, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(applaud, 33000000, 'Judgment', ['Agent': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(assault, 33000000, 'Attack', ['Agent': 'Assailant', 'Theme': 'Victim', 'Predicate': 'Reason']).
fnpattern(assault, 33000000, 'Attack', ['Agent': 'Assailant', 'Theme': 'Victim', 'Predicate': 'Purpose']).
fnpattern(attack, 33000000, 'Attack', ['Agent': 'Assailant', 'Theme': 'Victim', 'Predicate': 'Reason']).
fnpattern(attack, 33000000, 'Attack', ['Agent': 'Assailant', 'Theme': 'Victim', 'Predicate': 'Purpose']).
fnpattern(belittle, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(blame, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(castigate, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(censure, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(chastise, 33000000, 'Judgment_direct_address', ['Agent': 'Communicator', 'Theme': 'Addressee', 'Predicate': 'Reason']).
fnpattern(chide, 33000000, 'Judgment_direct_address', ['Agent': 'Communicator', 'Theme': 'Addressee', 'Predicate': 'Reason']).
fnpattern(commend, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(compliment, 33000000, 'Judgment_direct_address', ['Agent': 'Communicator', 'Theme': 'Addressee', 'Predicate': 'Reason']).
fnpattern(condemn, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(condone, 33000000, 'Forgiveness', ['Agent': 'Judge', 'Theme': 'Evaluee', 'Predicate': 'Offense']).
fnpattern(criticize, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(decry, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(denigrate, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(denounce, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(deprecate, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(deride, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(disparage, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(doubt, 33000000, 'Certainty', ['Agent': 'Cognizer', 'Theme': 'Content']).
fnpattern(doubt, 33000000, 'Certainty', ['Agent': 'Cognizer', 'Theme': 'Topic']).
fnpattern(excoriate, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(excuse, 33000000, 'Forgiveness', ['Agent': 'Judge', 'Theme': 'Evaluee', 'Predicate': 'Offense']).
fnpattern(extol, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(fault, 33000000, 'Judgment', ['Agent': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(forgive, 33000000, 'Forgiveness', ['Agent': 'Judge', 'Theme': 'Evaluee', 'Predicate': 'Offense']).
fnpattern(gibe, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(herald, 33000000, 'Heralding', ['Agent': 'Communicator', 'Theme': 'Event']).
fnpattern(herald, 33000000, 'Heralding', ['Agent': 'Communicator', 'Theme': 'Individual']).
fnpattern(indict, 33000000, 'Notification_of_charges', ['Agent': 'Arraign_authority', 'Theme': 'Accused', 'Predicate': 'Charges']).
fnpattern(indict, 33000000, 'Notification_of_charges', ['Agent': 'Arraign_authority', 'Theme': 'Accused', 'Predicate': 'Containing_event']).
fnpattern(laud, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(mock, 33000000, 'Judgment', ['Agent': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(mock, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(pardon, 33000000, 'Forgiveness', ['Agent': 'Judge', 'Theme': 'Evaluee', 'Predicate': 'Offense']).
fnpattern(praise, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(punish, 33000000, 'Rewards_and_punishments', ['Agent': 'Agent', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(rebuke, 33000000, 'Judgment_direct_address', ['Agent': 'Communicator', 'Theme': 'Addressee', 'Predicate': 'Reason']).
fnpattern(recompense, 33000000, 'Rewards_and_punishments', ['Agent': 'Agent', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(reprimand, 33000000, 'Judgment_direct_address', ['Agent': 'Communicator', 'Theme': 'Addressee', 'Predicate': 'Reason']).
fnpattern(reproach, 33000000, 'Judgment_direct_address', ['Agent': 'Communicator', 'Theme': 'Addressee', 'Predicate': 'Reason']).
fnpattern(reprove, 33000000, 'Judgment_direct_address', ['Agent': 'Communicator', 'Theme': 'Addressee', 'Predicate': 'Reason']).
fnpattern(reward, 33000000, 'Rewards_and_punishments', ['Agent': 'Agent', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(ridicule, 33000000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(sanction, 33000000, 'Grant_permission', ['Agent': 'Grantor', 'Theme': 'Grantee', 'Predicate': 'Action']).
fnpattern(scold, 33000000, 'Judgment_direct_address', ['Agent': 'Communicator', 'Theme': 'Addressee', 'Predicate': 'Reason']).
fnpattern(scorn, 33000000, 'Judgment', ['Agent': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(thank, 33000000, 'Judgment_direct_address', ['Agent': 'Communicator', 'Theme': 'Addressee', 'Predicate': 'Reason']).
fnpattern(upbraid, 33000000, 'Judgment_direct_address', ['Agent': 'Communicator', 'Theme': 'Addressee', 'Predicate': 'Reason']).
fnpattern(analyse, 34000000, 'Scrutiny', ['Agent': 'Cognizer', 'Theme': 'Ground', 'Attribute': 'Phenomenon']).
fnpattern(assess, 34000000, 'Assessing', ['Agent': 'Assessor', 'Theme': 'Phenomenon', 'Attribute': 'Feature']).
fnpattern(evaluate, 34000000, 'Assessing', ['Agent': 'Assessor', 'Theme': 'Phenomenon', 'Attribute': 'Feature']).
fnpattern(scrutinize, 34000000, 'Scrutiny', ['Agent': 'Cognizer', 'Theme': 'Ground', 'Attribute': 'Phenomenon']).
fnpattern(study, 34000000, 'Scrutiny', ['Agent': 'Cognizer', 'Theme': 'Ground', 'Attribute': 'Phenomenon']).
fnpattern(feel, 35010000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(hunt, 35010000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(check, 35020000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(comb, 35020000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(probe, 35020000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(scour, 35020000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(scout, 35020000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(search, 35020000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(sift, 35020000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(watch, 35020000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(examine, 35040000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(frisk, 35040000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(inspect, 35040000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(investigate, 35040000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(ransack, 35040000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(scan, 35040000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(scrutinize, 35040000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(survey, 35040000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(test, 35040000, 'Operational_testing', ['Agent': 'Tester', 'Location': 'Product', 'Theme': 'Unwanted_characteristics']).
fnpattern(test, 35040000, 'Operational_testing', ['Agent': 'Tester', 'Location': 'Tested_property', 'Theme': 'Unwanted_characteristics']).
fnpattern(forage, 35050000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(fumble, 35050000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(grope, 35050000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(listen, 35050000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(look, 35050000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(rummage, 35050000, 'Scrutiny', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Phenomenon']).
fnpattern(nose, 35060000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(seek, 35060000, 'Seeking', ['Agent': 'Cognizer', 'Location': 'Ground', 'Theme': 'Sought_entity']).
fnpattern(intermix, 36010000, 'Amalgamation', ['Actor1': 'Parts', 'Actor2': 'Part_2', 'Theme': 'Whole']).
fnpattern(intermix, 36010000, 'Amalgamation', ['Actor1': 'Part_1', 'Actor2': 'Part_2', 'Theme': 'Whole']).
fnpattern(agree, 36010100, 'Compatibility', ['Actor1': 'Item1', 'Actor2': 'Item2', 'Theme': 'Parameter']).
fnpattern(collaborate, 36010100, 'Collaboration', ['Actor1': 'Partner1', 'Actor2': 'Partner2', 'Theme': 'Undertaking']).
fnpattern(cooperate, 36010100, 'Collaboration', ['Actor1': 'Partner1', 'Actor2': 'Partner2', 'Theme': 'Undertaking']).
fnpattern(argue, 36010200, 'Quarreling', ['Actor1': 'Arguer1', 'Actor2': 'Arguer2', 'Theme': 'Issue']).
fnpattern(bicker, 36010200, 'Quarreling', ['Actor1': 'Arguer1', 'Actor2': 'Arguer2', 'Theme': 'Issue']).
fnpattern(brawl, 36010200, 'Hostile_encounter', ['Actor1': 'Side1', 'Actor2': 'Side2', 'Theme': 'Issue']).
fnpattern(clash, 36010200, 'Hostile_encounter', ['Actor1': 'Side1', 'Actor2': 'Side2', 'Theme': 'Issue']).
fnpattern(compete, 36010200, 'Competition', ['Actor1': 'Participant1', 'Actor2': 'Participant2', 'Theme': 'Competition']).
fnpattern(duel, 36010200, 'Hostile_encounter', ['Actor1': 'Side1', 'Actor2': 'Side2', 'Theme': 'Issue']).
fnpattern(quarrel, 36010200, 'Quarreling', ['Actor1': 'Arguer1', 'Actor2': 'Arguer2', 'Theme': 'Issue']).
fnpattern(quibble, 36010200, 'Quarreling', ['Actor1': 'Arguer1', 'Actor2': 'Arguer2', 'Theme': 'Issue']).
fnpattern(scuffle, 36010200, 'Hostile_encounter', ['Actor1': 'Side1', 'Actor2': 'Side2', 'Theme': 'Issue']).
fnpattern(skirmish, 36010200, 'Hostile_encounter', ['Actor1': 'Side1', 'Actor2': 'Side2', 'Theme': 'Issue']).
fnpattern(squabble, 36010200, 'Quarreling', ['Actor1': 'Arguer1', 'Actor2': 'Arguer2', 'Theme': 'Issue']).
fnpattern(war, 36010200, 'Hostile_encounter', ['Actor1': 'Side1', 'Actor2': 'Side2', 'Theme': 'Issue']).
fnpattern(wrangle, 36010200, 'Quarreling', ['Actor1': 'Arguer1', 'Actor2': 'Arguer2', 'Theme': 'Issue']).
fnpattern(court, 36020000, 'Personal_relationship', ['Actor1': 'Partner1', 'Actor2': 'Partner2']).
fnpattern(date, 36020000, 'Personal_relationship', ['Actor1': 'Partner1', 'Actor2': 'Partner2']).
fnpattern(divorce, 36020000, 'Forming_relationships', ['Actor1': 'Partner1', 'Actor2': 'Partner2']).
fnpattern(marry, 36020000, 'Forming_relationships', ['Actor1': 'Partner1', 'Actor2': 'Partner2']).
fnpattern(play, 36030100, 'Competition', ['Actor1': 'Participant1', 'Actor2': 'Participant2']).
fnpattern(battle, 36030200, 'Hostile_encounter', ['Actor1': 'Side1', 'Actor2': 'Side2']).
fnpattern(fight, 36030200, 'Hostile_encounter', ['Actor1': 'Side1', 'Actor2': 'Side2']).
fnpattern(fight, 36030200, 'Quarreling', ['Actor1': 'Arguer1', 'Actor2': 'Arguer2']).
fnpattern(communicate, 36040100, 'Communication', ['Actor': 'Communicator', 'Actor2': 'Addressee', 'Topic': 'Message']).
fnpattern(communicate, 36040100, 'Communication', ['Actor': 'Communicator', 'Actor2': 'Addressee', 'Topic': 'Topic']).
fnpattern(communicate, 36040100, 'Communication', ['Actor1': 'Communicator', 'Actor2': 'Addressee', 'Topic': 'Message']).
fnpattern(communicate, 36040100, 'Communication', ['Actor1': 'Communicator', 'Actor2': 'Addressee', 'Topic': 'Topic']).
fnpattern(demonstrate, 37010000, 'Reasoning', ['Agent': 'Arguer', 'Topic': 'Content', 'Recipient': 'Addressee']).
fnpattern(explain, 37010000, 'Justifying', ['Agent': 'Agent', 'Topic': 'State_of_affairs']).
fnpattern(explain, 37010000, 'Justifying', ['Agent': 'Agent', 'Topic': 'Act']).
fnpattern(explain, 37010000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(explain, 37010000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(preach, 37010000, 'Speak_on_topic', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Audience']).
fnpattern(preach, 37010000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(preach, 37010000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(read, 37010100, 'Hear', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Hearer']).
fnpattern(read, 37010100, 'Hear', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Hearer']).
fnpattern(show, 37010110, 'Reasoning', ['Agent': 'Arguer', 'Topic': 'Content', 'Recipient': 'Addressee']).
fnpattern(tell, 37010111, 'Reporting', ['Agent': 'Informer', 'Topic': 'Behavior', 'Recipient': 'Authorities']).
fnpattern(tell, 37010111, 'Request', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(tell, 37010111, 'Request', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(tell, 37010111, 'Telling', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(tell, 37010111, 'Telling', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(write, 37010111, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Communication', 'Recipient': 'Addressee']).
fnpattern(write, 37010111, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(write, 37010111, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(write, 37010111, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(ask, 37010111, 'Questioning', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(ask, 37010111, 'Questioning', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(ask, 37010111, 'Request', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(ask, 37010111, 'Request', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(inquire, 37012000, 'Questioning', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(inquire, 37012000, 'Questioning', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(tell, 37020000, 'Reporting', ['Agent': 'Informer', 'Topic': 'Behavior', 'Recipient': 'Authorities']).
fnpattern(tell, 37020000, 'Request', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(tell, 37020000, 'Request', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(tell, 37020000, 'Telling', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(tell, 37020000, 'Telling', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(babble, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(babble, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(bark, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(bark, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(bawl, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(bawl, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(bellow, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(bellow, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(bleat, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(bleat, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(bray, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(bray, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(burble, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(burble, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(cackle, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(cackle, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(chant, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(chant, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(chatter, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(chatter, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(chirp, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(chirp, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(cluck, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(cluck, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(coo, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(coo, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(croak, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(croak, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(croon, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(croon, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(crow, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(crow, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(cry, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(cry, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(drawl, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(drawl, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(drone, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(drone, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(gabble, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(gabble, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(gibber, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(gibber, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(groan, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(groan, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(growl, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(growl, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(grumble, 37030000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(grumble, 37030000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(grunt, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(grunt, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(hiss, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(hiss, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(hoot, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(hoot, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(howl, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(howl, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(jabber, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(jabber, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(lisp, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(lisp, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(moan, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(moan, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(mumble, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(mumble, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(murmur, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(murmur, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(mutter, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(mutter, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(prattle, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(prattle, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(purr, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(purr, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(rasp, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(rasp, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(roar, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(roar, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(rumble, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(rumble, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(scream, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(scream, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(screech, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(screech, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(shout, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(shout, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(shriek, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(shriek, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(sing, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(sing, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(snarl, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(snarl, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(splutter, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(splutter, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(squawk, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(squawk, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(squeak, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(squeak, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(squeal, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(squeal, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(stammer, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(stammer, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(stutter, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(stutter, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(thunder, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(thunder, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(trill, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(trill, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(trumpet, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(trumpet, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(twitter, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(twitter, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(wail, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(wail, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(warble, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(warble, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(wheeze, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(wheeze, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(whimper, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(whimper, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(whine, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(whine, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(whisper, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(whisper, 37030000, 'Communication_manner', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(whoop, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(whoop, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(yell, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(yell, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(yelp, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(yelp, 37030000, 'Communication_noise', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(cable, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(cable, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(cable, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Communication', 'Recipient': 'Addressee']).
fnpattern(cable, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(e-mail, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Communication', 'Recipient': 'Addressee']).
fnpattern(e-mail, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(fax, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Communication', 'Recipient': 'Addressee']).
fnpattern(fax, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(phone, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(phone, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(phone, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Communication', 'Recipient': 'Addressee']).
fnpattern(phone, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(radio, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(radio, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(radio, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Communication', 'Recipient': 'Addressee']).
fnpattern(radio, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(semaphore, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(semaphore, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(telegraph, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(telegraph, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(telegraph, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Communication', 'Recipient': 'Addressee']).
fnpattern(telegraph, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(telephone, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(telephone, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(telephone, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Communication', 'Recipient': 'Addressee']).
fnpattern(telephone, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(telex, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(telex, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(telex, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Communication', 'Recipient': 'Addressee']).
fnpattern(telex, 37040000, 'Contacting', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(wire, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(wire, 37040000, 'Communication_means', ['Agent': 'Communicator', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(speak, 37050000, 'Statement', ['Actor1': 'Speaker', 'Topic': 'Message', 'Actor2': 'Addressee']).
fnpattern(speak, 37050000, 'Statement', ['Actor1': 'Speaker', 'Topic': 'Topic', 'Actor2': 'Addressee']).
fnpattern(talk, 37050000, 'Statement', ['Actor1': 'Speaker', 'Topic': 'Message', 'Actor2': 'Addressee']).
fnpattern(talk, 37050000, 'Statement', ['Actor1': 'Speaker', 'Topic': 'Topic', 'Actor2': 'Addressee']).
fnpattern(chat, 37060000, 'Chatting', ['Actor1': 'Interlocutor1', 'Actor2': 'Interlocutor2', 'Topic': 'Topic']).
fnpattern(converse, 37060000, 'Chatting', ['Actor1': 'Interlocutor1', 'Actor2': 'Interlocutor2', 'Topic': 'Topic']).
fnpattern(gab, 37060000, 'Chatting', ['Actor1': 'Interlocutor1', 'Actor2': 'Interlocutor2', 'Topic': 'Topic']).
fnpattern(gossip, 37060000, 'Chatting', ['Actor1': 'Interlocutor1', 'Actor2': 'Interlocutor2', 'Topic': 'Topic']).
fnpattern(announce, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(announce, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(claim, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(claim, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(confess, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(confess, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(confide, 37070000, 'Telling', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(confide, 37070000, 'Telling', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(convey, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(convey, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(declare, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(declare, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(disclose, 37070000, 'Reveal_secret', ['Agent': 'Speaker', 'Topic': 'Information', 'Recipient': 'Addressee']).
fnpattern(exclaim, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(exclaim, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(insist, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(insist, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(mention, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(mention, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(proclaim, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(proclaim, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(propose, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(propose, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(recount, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(recount, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(reiterate, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(reiterate, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(relate, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(relate, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(reply, 37070000, 'Communication_response', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(reply, 37070000, 'Communication_response', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(reply, 37070000, 'Communication_response', ['Agent': 'Speaker', 'Topic': 'Trigger', 'Recipient': 'Addressee']).
fnpattern(respond, 37070000, 'Communication_response', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(respond, 37070000, 'Communication_response', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(respond, 37070000, 'Communication_response', ['Agent': 'Speaker', 'Topic': 'Trigger', 'Recipient': 'Addressee']).
fnpattern(retort, 37070000, 'Communication_response', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(retort, 37070000, 'Communication_response', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(retort, 37070000, 'Communication_response', ['Agent': 'Speaker', 'Topic': 'Trigger', 'Recipient': 'Addressee']).
fnpattern(reveal, 37070000, 'Reveal_secret', ['Agent': 'Speaker', 'Topic': 'Information', 'Recipient': 'Addressee']).
fnpattern(say, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(say, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(state, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(state, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(suggest, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(suggest, 37070000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(utter, 37070000, 'Text_creation', ['Agent': 'Author', 'Topic': 'Text', 'Recipient': 'Addressee']).
fnpattern(voice, 37070000, 'Expressing_publicly', ['Agent': 'Communucator', 'Topic': 'Content', 'Recipient': 'Addressee']).
fnpattern(remark, 37070100, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(remark, 37070100, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(report, 37070100, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(report, 37070100, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(boast, 37080000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(boast, 37080000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(brag, 37080000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(brag, 37080000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(complain, 37080000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(complain, 37080000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(gripe, 37080000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(gripe, 37080000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(grumble, 37080000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(grumble, 37080000, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(assure, 37090000, 'Telling', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(assure, 37090000, 'Telling', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(inform, 37090000, 'Telling', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(inform, 37090000, 'Telling', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(notify, 37090000, 'Telling', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(notify, 37090000, 'Telling', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(admonish, 37090100, 'Attempt_suasion', ['Agent': 'Speaker', 'Topic': 'Content', 'Recipient': 'Addressee']).
fnpattern(admonish, 37090100, 'Attempt_suasion', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(advise, 37090100, 'Telling', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(advise, 37090100, 'Telling', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(caution, 37090100, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(caution, 37090100, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(lecture, 37110100, 'Speak_on_topic', ['Agent': 'Speaker', 'Topic': 'Content', 'Recipient': 'Audience']).
fnpattern(comment, 37110110, 'Statement', ['Agent': 'Speaker', 'Topic': 'Message', 'Recipient': 'Addressee']).
fnpattern(comment, 37110110, 'Statement', ['Agent': 'Speaker', 'Topic': 'Topic', 'Recipient': 'Addressee']).
fnpattern(bark, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(bark, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(bark, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(bark, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(bark, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(bark, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(bellow, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(bellow, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(bellow, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(bellow, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(bellow, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(bellow, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(bleat, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(bleat, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(bleat, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(bleat, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(bleat, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(bleat, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(bray, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(bray, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(bray, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(bray, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(bray, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(bray, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(cackle, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(cackle, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(cackle, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(cackle, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(cackle, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(cackle, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(caw, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(caw, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(chatter, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(chatter, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(cheep, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(cheep, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(chirp, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(chirp, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(chirp, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(chirp, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(chirp, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(chirp, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(chirrup, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(chirrup, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(chirrup, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(chirrup, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(cluck, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(cluck, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(cluck, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(cluck, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(coo, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(coo, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(coo, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(coo, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(coo, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(coo, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(croak, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(croak, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(croak, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(croak, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(croak, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(croak, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(crow, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(crow, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(crow, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(crow, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(drone, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(drone, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(drone, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(drone, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(drone, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(drone, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(gobble, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(gobble, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(groan, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(groan, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(groan, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(groan, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(growl, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(growl, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(growl, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(growl, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(growl, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(growl, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(grunt, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(grunt, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(grunt, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(grunt, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(grunt, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(grunt, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(hiss, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(hiss, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(hiss, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(hiss, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(hiss, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(hiss, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(hoot, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(hoot, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(hoot, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(hoot, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(hoot, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(hoot, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(howl, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(howl, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(howl, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(howl, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(howl, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(howl, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(mew, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(mew, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(moan, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(moan, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(moan, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(moan, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(moan, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(moan, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(moo, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(moo, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(neigh, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(neigh, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(peep, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(peep, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(purr, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(purr, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(purr, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(purr, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(purr, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(purr, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(quack, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(quack, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(rattle, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(rattle, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(rattle, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(rattle, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(rattle, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(rattle, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(roar, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(roar, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(roar, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(roar, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(roar, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(roar, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(rumble, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(rumble, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(rumble, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(rumble, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(squawk, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(squawk, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(squawk, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(squawk, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(squawk, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(squawk, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(squeak, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(squeak, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(squeak, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(squeak, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(squeak, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(squeak, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(squeal, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(squeal, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(squeal, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(squeal, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(squeal, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(squeal, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(thunder, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(thunder, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(thunder, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(thunder, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(thunder, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(thunder, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(trill, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(trill, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(trill, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(trill, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(tweet, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(tweet, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(twitter, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(twitter, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(twitter, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(twitter, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(twitter, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(twitter, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(ululate, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(ululate, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(wail, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(wail, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(wail, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(wail, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(wail, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(wail, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(warble, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(warble, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(warble, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(warble, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(wheeze, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(wheeze, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(wheeze, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(wheeze, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(whimper, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(whimper, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(whimper, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(whimper, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(whimper, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(whimper, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(whine, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(whine, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(whine, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(whine, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(whine, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(whine, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(whinny, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(whinny, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(whistle, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(whistle, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(yap, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(yap, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(yell, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(yell, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(yell, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(yell, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(yelp, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(yelp, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(yelp, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(yelp, 38000000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(yelp, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(yelp, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(yowl, 38000000, 'Make_noise', ['Agent': 'Noisy_event', 'Theme': 'Sound']).
fnpattern(yowl, 38000000, 'Make_noise', ['Agent': 'Sound_source', 'Theme': 'Sound']).
fnpattern(eat, 39010100, 'Ingestion', ['Agent': 'Ingestor', 'Instrument': 'Instrument', 'Source': 'Ingestibles']).
fnpattern(drink, 39010200, 'Ingestion', ['Agent': 'Ingestor', 'Instrument': 'Instrument', 'Source': 'Ingestibles']).
fnpattern(masticate, 39020100, 'Grinding', ['Agent': 'Grinder', 'Patient': 'Undergoer']).
fnpattern(munch, 39020100, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(nibble, 39020100, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(sip, 39020200, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(slurp, 39020200, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(gobble, 39030100, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(gulp, 39030200, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(guzzle, 39030200, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(quaff, 39030200, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(swig, 39030200, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(consume, 39040100, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(devour, 39040100, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(ingest, 39040100, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(imbibe, 39040200, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(swill, 39040200, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(breakfast, 39050000, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(dine, 39050000, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(feast, 39050000, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(lunch, 39050000, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(nosh, 39050000, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(snack, 39050000, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(sup, 39050000, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(feed, 39060000, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(feed, 39070000, 'Ingestion', ['Agent': 'Ingestor', 'Patient': 'Ingestibles']).
fnpattern(belch, 40011000, 'Excreting', ['Agent': 'Excreter']).
fnpattern(burp, 40011000, 'Excreting', ['Agent': 'Excreter']).
fnpattern(fart, 40011000, 'Excreting', ['Agent': 'Excreter']).
fnpattern(dribble, 40012000, 'Fluidic_motion', ['Theme': 'Fluid']).
fnpattern(puke, 40012000, 'Excreting', ['Agent': 'Excreter', 'Theme': 'Excreta']).
fnpattern(sweat, 40012000, 'Excreting', ['Agent': 'Excreter', 'Theme': 'Excreta']).
fnpattern(vomit, 40012000, 'Excreting', ['Agent': 'Excreter', 'Theme': 'Excreta']).
fnpattern(breathe, 40012100, 'Breathing', ['Agent': 'Agent', 'Theme': 'Air']).
fnpattern(defecate, 40012100, 'Excreting', ['Agent': 'Excreter', 'Theme': 'Excreta']).
fnpattern(retch, 40012100, 'Excreting', ['Agent': 'Excreter', 'Theme': 'Excreta']).
fnpattern(exhale, 40013100, 'Breathing', ['Agent': 'Agent', 'Theme': 'Air']).
fnpattern(exhale, 40013100, 'Emitting', ['Agent': 'Source_emitter', 'Theme': 'Emission']).
fnpattern(inhale, 40013200, 'Breathing', ['Agent': 'Agent', 'Theme': 'Air']).
fnpattern(cackle, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(cackle, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(cackle, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(cackle, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(chuckle, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(chuckle, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(chuckle, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(chuckle, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(cry, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(cry, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(cry, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(cry, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(frown, 40020000, 'Making_faces', ['Agent': 'Agent', 'Cause': 'Internal_cause']).
fnpattern(gasp, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(gasp, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(gasp, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(gasp, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(grimace, 40020000, 'Making_faces', ['Agent': 'Agent', 'Cause': 'Internal_cause']).
fnpattern(grin, 40020000, 'Making_faces', ['Agent': 'Agent', 'Cause': 'Internal_cause']).
fnpattern(groan, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(groan, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(groan, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(groan, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(growl, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(growl, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(growl, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(growl, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(howl, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(howl, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(howl, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(howl, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(moan, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(moan, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(moan, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(moan, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(pout, 40020000, 'Making_faces', ['Agent': 'Agent', 'Cause': 'Internal_cause']).
fnpattern(scoff, 40020000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Cause': 'Topic', 'Recipient': 'Addressee']).
fnpattern(scoff, 40020000, 'Judgment_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Cause': 'Reason', 'Recipient': 'Addressee']).
fnpattern(scowl, 40020000, 'Making_faces', ['Agent': 'Agent', 'Cause': 'Internal_cause']).
fnpattern(simper, 40020000, 'Communication_manner', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(simper, 40020000, 'Communication_manner', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(simper, 40020000, 'Communication_manner', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(simper, 40020000, 'Communication_manner', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(smile, 40020000, 'Making_faces', ['Agent': 'Agent', 'Cause': 'Internal_cause']).
fnpattern(smirk, 40020000, 'Making_faces', ['Agent': 'Agent', 'Cause': 'Internal_cause']).
fnpattern(snort, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(snort, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(snort, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(snort, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(titter, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Cause': 'Addressee']).
fnpattern(titter, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(titter, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Cause': 'Addressee']).
fnpattern(titter, 40020000, 'Communication_noise', ['Agent': 'Speaker', 'Theme': 'Topic', 'Recipient': 'Addressee']).
fnpattern(wag, 40031000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(beckon, 40031100, 'Gesture', ['Agent': 'Communicator', 'Patient': 'Body_part', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(beckon, 40031100, 'Gesture', ['Agent': 'Communicator', 'Patient': 'Body_part', 'Theme': 'Indicated_entity', 'Recipient': 'Addressee']).
fnpattern(blink, 40031100, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(clap, 40031100, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(gesture, 40031100, 'Gesture', ['Agent': 'Communicator', 'Patient': 'Body_part', 'Theme': 'Message', 'Recipient': 'Addressee']).
fnpattern(gesture, 40031100, 'Gesture', ['Agent': 'Communicator', 'Patient': 'Body_part', 'Theme': 'Indicated_entity', 'Recipient': 'Addressee']).
fnpattern(nod, 40031100, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(shrug, 40031100, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(wave, 40031100, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(wink, 40031100, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(arch, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(bat, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(cock, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(crane, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(cross, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(flap, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(flex, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(flutter, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(gnash, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(grind, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(hang, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(hunch, 40032000, 'Posture', ['Agent': 'Agent']).
fnpattern(pucker, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(purse, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(roll, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(shake, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(shuffle, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(smack, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(stamp, 40032000, 'Self_motion', ['Agent': 'Self_mover']).
fnpattern(stretch, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(toss, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(twitch, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(wag, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(waggle, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(wiggle, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(wrinkle, 40032000, 'Body_movement', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(bob, 40033000, 'Body_movement', ['Agent': 'Agent']).
fnpattern(kneel, 40033000, 'Body_movement', ['Agent': 'Agent']).
fnpattern(catnap, 40040000, 'Sleep', ['Agent': 'Sleeper']).
fnpattern(doze, 40040000, 'Sleep', ['Agent': 'Sleeper']).
fnpattern(drowse, 40040000, 'Sleep', ['Agent': 'Sleeper']).
fnpattern(nap, 40040000, 'Sleep', ['Agent': 'Sleeper']).
fnpattern(sleep, 40040000, 'Sleep', ['Agent': 'Sleeper']).
fnpattern(slumber, 40040000, 'Sleep', ['Agent': 'Sleeper']).
fnpattern(snooze, 40040000, 'Sleep', ['Agent': 'Sleeper']).
fnpattern(shake, 40060000, 'Body_movement', ['Experiencer': 'Agent']).
fnpattern(shiver, 40060000, 'Body_movement', ['Experiencer': 'Agent']).
fnpattern(shudder, 40060000, 'Body_movement', ['Experiencer': 'Agent']).
fnpattern(writhe, 40060000, 'Body_movement', ['Experiencer': 'Agent']).
fnpattern(asphyxiate, 40070000, 'Death', ['Agent': 'Cause', 'Theme': 'Protagonist']).
fnpattern(drown, 40070000, 'Death', ['Agent': 'Cause', 'Theme': 'Protagonist']).
fnpattern(starve, 40070000, 'Death', ['Agent': 'Cause', 'Theme': 'Protagonist']).
fnpattern(starve, 40070000, 'Killing', ['Agent': 'Cause', 'Theme': 'Victim']).
fnpattern(starve, 40070000, 'Killing', ['Agent': 'Killer', 'Theme': 'Victim']).
fnpattern(suffocate, 40070000, 'Death', ['Agent': 'Cause', 'Theme': 'Protagonist']).
fnpattern(hurt, 40081000, 'Perception_body', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(itch, 40081000, 'Perception_body', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(ache, 40082000, 'Perception_body', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(burn, 40082000, 'Perception_body', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(prickle, 40082000, 'Perception_body', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(smart, 40082000, 'Perception_body', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(sting, 40082000, 'Perception_body', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(tickle, 40082000, 'Perception_body', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(tingle, 40082000, 'Perception_body', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(stub, 40083100, 'Experience_bodily_harm', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(break, 40083110, 'Experience_bodily_harm', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(pull, 40083110, 'Experience_bodily_harm', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(sprain, 40083110, 'Experience_bodily_harm', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(twist, 40083110, 'Experience_bodily_harm', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(burn, 40083200, 'Experience_bodily_harm', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(cut, 40083200, 'Experience_bodily_harm', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(hurt, 40083200, 'Experience_bodily_harm', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(injure, 40083200, 'Experience_bodily_harm', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(strain, 40083200, 'Experience_bodily_harm', ['Experiencer': 'Experiencer', 'Patient': 'Body_part']).
fnpattern(bathe, 41011000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(bathe, 41011000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(shave, 41011000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(shave, 41011000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(shower, 41011000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(shower, 41011000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(wash, 41011000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(wash, 41011000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(groom, 41012000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(groom, 41012000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(floss, 41021000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(floss, 41021000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(shave, 41021000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(shave, 41021000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(wash, 41021000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(wash, 41021000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(comb, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(comb, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(curl, 41022000, 'Hair_configuration', ['Patient': 'Hair']).
fnpattern(manicure, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(manicure, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(plait, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(plait, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(pluck, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(pluck, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(shampoo, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(shampoo, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(soap, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(soap, 41022000, 'Grooming', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(assassinate, 42010000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(assassinate, 42010000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(butcher, 42010000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(butcher, 42010000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(eliminate, 42010000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(eliminate, 42010000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(execute, 42010000, 'Execution', ['Agent': 'Executioner', 'Patient': 'Executed']).
fnpattern(liquidate, 42010000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(liquidate, 42010000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(massacre, 42010000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(massacre, 42010000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(murder, 42010000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(murder, 42010000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(slaughter, 42010000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(slaughter, 42010000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(slay, 42010000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(slay, 42010000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(kill, 42010100, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(kill, 42010100, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(asphyxiate, 42020000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(asphyxiate, 42020000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(crucify, 42020000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(crucify, 42020000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(drown, 42020000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(drown, 42020000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(electrocute, 42020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(electrocute, 42020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(electrocute, 42020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(electrocute, 42020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(garrotte, 42020000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(garrotte, 42020000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(hang, 42020000, 'Execution', ['Agent': 'Executioner', 'Patient': 'Executed']).
fnpattern(knife, 42020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(knife, 42020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(knife, 42020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(knife, 42020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(smother, 42020000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(smother, 42020000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(stab, 42020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Victim']).
fnpattern(stab, 42020000, 'Cause_harm', ['Agent': 'Agent', 'Patient': 'Body_part']).
fnpattern(stab, 42020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Victim']).
fnpattern(stab, 42020000, 'Cause_harm', ['Agent': 'Cause', 'Patient': 'Body_part']).
fnpattern(suffocate, 42020000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(suffocate, 42020000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(flame, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(flame, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(flame, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(flare, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(flare, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(flare, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(flash, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(flash, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(flash, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(flicker, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(flicker, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(flicker, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(gleam, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(gleam, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(gleam, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(glimmer, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(glimmer, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(glimmer, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(glint, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(glint, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(glint, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(glisten, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(glisten, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(glisten, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(glitter, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(glitter, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(glitter, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(glow, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(glow, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(glow, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(scintillate, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(scintillate, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(scintillate, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(shimmer, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(shimmer, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(shimmer, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(shine, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(shine, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(shine, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(sparkle, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(sparkle, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(sparkle, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(twinkle, 43010000, 'Light_movement', ['Agent': 'Emitter', 'Theme': 'Beam', 'Location': 'Source']).
fnpattern(twinkle, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Figure', 'Location': 'Ground']).
fnpattern(twinkle, 43010000, 'Location_of_light', ['Agent': 'Cause_to_shine', 'Theme': 'Light', 'Location': 'Ground']).
fnpattern(bang, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(buzz, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(chug, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(clack, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(clang, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(clank, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(clatter, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(click, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(clink, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(clump, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(clunk, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(crackle, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(crash, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(creak, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(crunch, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(fizz, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(gurgle, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(hiss, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(howl, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(patter, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(ping, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(purr, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(putter, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(roar, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(rumble, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(rustle, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(screech, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(splash, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(splutter, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(squelch, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(swish, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(thud, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(thump, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(thunder, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(wheeze, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(whine, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(whir, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(zing, 43020000, 'Motion_noise', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(reek, 43030000, 'Chemical-sense_description', ['Theme': 'Sensory_attribute', 'Location': 'Source']).
fnpattern(smell, 43030000, 'Chemical-sense_description', ['Theme': 'Sensory_attribute', 'Location': 'Source']).
fnpattern(stink, 43030000, 'Chemical-sense_description', ['Theme': 'Sensory_attribute', 'Location': 'Source']).
fnpattern(belch, 43040000, 'Excreting', ['Theme': 'Excreta', 'Source': 'Excreter']).
fnpattern(bubble, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(bubble, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(dribble, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(dribble, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(drip, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(drip, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(emanate, 43040000, 'Emanating', ['Theme': 'Emission', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(emanate, 43040000, 'Emanating', ['Theme': 'Emission', 'Source': 'Source', 'Location': 'Path']).
fnpattern(exude, 43040000, 'Emanating', ['Theme': 'Emission', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(exude, 43040000, 'Emanating', ['Theme': 'Emission', 'Source': 'Source', 'Location': 'Path']).
fnpattern(exude, 43040000, 'Emitting', ['Theme': 'Emission', 'Source': 'Source_emitter', 'Location': 'Goal']).
fnpattern(exude, 43040000, 'Emitting', ['Theme': 'Emission', 'Source': 'Source_emitter', 'Location': 'Path']).
fnpattern(gush, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(gush, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(leak, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(leak, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(ooze, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(ooze, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(pour, 43040000, 'Mass_motion', ['Theme': 'Mass_theme', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(pour, 43040000, 'Mass_motion', ['Theme': 'Mass_theme', 'Source': 'Source', 'Location': 'Area']).
fnpattern(pour, 43040000, 'Mass_motion', ['Theme': 'Mass_theme', 'Source': 'Source', 'Location': 'Path']).
fnpattern(radiate, 43040000, 'Emanating', ['Theme': 'Emission', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(radiate, 43040000, 'Emanating', ['Theme': 'Emission', 'Source': 'Source', 'Location': 'Path']).
fnpattern(radiate, 43040000, 'Emitting', ['Theme': 'Emission', 'Source': 'Source_emitter', 'Location': 'Goal']).
fnpattern(radiate, 43040000, 'Emitting', ['Theme': 'Emission', 'Source': 'Source_emitter', 'Location': 'Path']).
fnpattern(seep, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(seep, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(spew, 43040000, 'Excreting', ['Theme': 'Excreta', 'Source': 'Excreter']).
fnpattern(spew, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(spew, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(spill, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(spill, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(spout, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(spout, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(spurt, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(spurt, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(squirt, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(squirt, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(stream, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(stream, 43040000, 'Fluidic_motion', ['Theme': 'Fluid', 'Source': 'Source', 'Location': 'Area']).
fnpattern(stream, 43040000, 'Mass_motion', ['Theme': 'Mass_theme', 'Source': 'Source', 'Location': 'Goal']).
fnpattern(stream, 43040000, 'Mass_motion', ['Theme': 'Mass_theme', 'Source': 'Source', 'Location': 'Area']).
fnpattern(stream, 43040000, 'Mass_motion', ['Theme': 'Mass_theme', 'Source': 'Source', 'Location': 'Path']).
fnpattern(sweat, 43040000, 'Excreting', ['Theme': 'Excreta', 'Source': 'Excreter']).
fnpattern(annihilate, 44000000, 'Destroying', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(annihilate, 44000000, 'Destroying', ['Agent': 'Destroyer', 'Patient': 'Undergoer']).
fnpattern(damage, 44000000, 'Damaging', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(damage, 44000000, 'Damaging', ['Agent': 'Cause', 'Patient': 'Patient']).
fnpattern(demolish, 44000000, 'Destroying', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(demolish, 44000000, 'Destroying', ['Agent': 'Destroyer', 'Patient': 'Undergoer']).
fnpattern(destroy, 44000000, 'Destroying', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(destroy, 44000000, 'Destroying', ['Agent': 'Destroyer', 'Patient': 'Undergoer']).
fnpattern(devastate, 44000000, 'Destroying', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(devastate, 44000000, 'Destroying', ['Agent': 'Destroyer', 'Patient': 'Undergoer']).
fnpattern(obliterate, 44000000, 'Destroying', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(obliterate, 44000000, 'Destroying', ['Agent': 'Destroyer', 'Patient': 'Undergoer']).
fnpattern(raze, 44000000, 'Destroying', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(raze, 44000000, 'Destroying', ['Agent': 'Destroyer', 'Patient': 'Undergoer']).
fnpattern(shatter, 44000000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient': 'Whole_patient']).
fnpattern(break, 45010000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(chip, 45010000, 'Damaging', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(fracture, 45010000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(rip, 45010000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(shatter, 45010000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(smash, 45010000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(snap, 45010000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(splinter, 45010000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(split, 45010000, 'Separation', ['Agent': 'Agent', 'Patient': 'Whole', 'Oblique': 'Parts']).
fnpattern(split, 45010000, 'Separation', ['Agent': 'Cause', 'Patient': 'Whole', 'Oblique': 'Parts']).
fnpattern(tear, 45010000, 'Cause_to_fragment', ['Agent': 'Agent', 'Patient': 'Patient']).
fnpattern(bend, 45020000, 'Reshaping', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(bend, 45020000, 'Reshaping', ['Agent': 'Deformer', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(crumple, 45020000, 'Reshaping', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(crumple, 45020000, 'Reshaping', ['Agent': 'Deformer', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(fold, 45020000, 'Reshaping', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(fold, 45020000, 'Reshaping', ['Agent': 'Deformer', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(bake, 45030000, 'Absorb_heat', ['Agent': 'Heat_source', 'Patient': 'Entity']).
fnpattern(bake, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(bake, 45030000, 'Cooking_creation', ['Agent': 'Cook', 'Patient': 'Ingredients', 'Instrument': 'Heating_instrument']).
fnpattern(blanch, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(boil, 45030000, 'Absorb_heat', ['Agent': 'Heat_source', 'Patient': 'Entity']).
fnpattern(boil, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(braise, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(broil, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(brown, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(coddle, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(cook, 45030000, 'Absorb_heat', ['Agent': 'Heat_source', 'Patient': 'Entity']).
fnpattern(cook, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(cook, 45030000, 'Cooking_creation', ['Agent': 'Cook', 'Patient': 'Ingredients', 'Instrument': 'Heating_instrument']).
fnpattern(fry, 45030000, 'Absorb_heat', ['Agent': 'Heat_source', 'Patient': 'Entity']).
fnpattern(fry, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(grill, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(microwave, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(parboil, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(poach, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(reheat, 45030000, 'Cause_temperature_change', ['Agent': 'Agent', 'Patient': 'Item', 'Instrument': 'Hot_Cold_source']).
fnpattern(reheat, 45030000, 'Cause_temperature_change', ['Agent': 'Cause', 'Patient': 'Item', 'Instrument': 'Hot_Cold_source']).
fnpattern(reheat, 45030000, 'Inchoative_change_of_temperature', ['Agent': 'Cause', 'Patient': 'Item', 'Instrument': 'Hot_Cold_source']).
fnpattern(roast, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(saute, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(scald, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(simmer, 45030000, 'Absorb_heat', ['Agent': 'Heat_source', 'Patient': 'Entity']).
fnpattern(simmer, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(steam, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(stew, 45030000, 'Absorb_heat', ['Agent': 'Heat_source', 'Patient': 'Entity']).
fnpattern(stew, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(toast, 45030000, 'Apply_heat', ['Agent': 'Cook', 'Patient': 'Food', 'Instrument': 'Heating_instrument']).
fnpattern(abrade, 45040000, 'Experience_bodily_harm', ['Agent': 'Experiencer', 'Patient': 'Body_part', 'Instrument': 'Injuring_entity']).
fnpattern(advance, 45040000, 'Change_position_on_a_scale', ['Patient': 'Attribute']).
fnpattern(awake, 45040000, 'Waking_up', ['Patient': 'Sleeper']).
fnpattern(awaken, 45040000, 'Cause_to_wake', ['Agent': 'Agent', 'Patient': 'Sleeper', 'Instrument': 'Instrument']).
fnpattern(awaken, 45040000, 'Cause_to_wake', ['Agent': 'Cause', 'Patient': 'Sleeper', 'Instrument': 'Instrument']).
fnpattern(bisect, 45040000, 'Separation', ['Agent': 'Agent', 'Patient': 'Whole', 'Instrument': 'Instrument']).
fnpattern(bisect, 45040000, 'Separation', ['Agent': 'Cause', 'Patient': 'Whole', 'Instrument': 'Instrument']).
fnpattern(botch, 45040000, 'Bungling', ['Agent': 'Agent', 'Patient': 'Patient', 'Instrument': 'Instrument']).
fnpattern(botch, 45040000, 'Bungling', ['Agent': 'Agent', 'Patient': 'Action', 'Instrument': 'Instrument']).
fnpattern(categorize, 45040000, 'Categorization', ['Agent': 'Cognizer', 'Patient': 'Item']).
fnpattern(chill, 45040000, 'Cause_temperature_change', ['Agent': 'Cause', 'Patient': 'Item', 'Instrument': 'Hot_Cold_source']).
fnpattern(chill, 45040000, 'Cause_temperature_change', ['Agent': 'Agent', 'Patient': 'Item', 'Instrument': 'Hot_Cold_source']).
fnpattern(chill, 45040000, 'Inchoative_change_of_temperature', ['Patient': 'Item']).
fnpattern(circumcise, 45040000, 'Rite', ['Agent': 'Leader', 'Patient': 'Member', 'Instrument': 'Instrument']).
fnpattern(circumcise, 45040000, 'Rite', ['Agent': 'Leader', 'Patient': 'Object', 'Instrument': 'Instrument']).
fnpattern(circumcise, 45040000, 'Rite', ['Agent': 'Guardian', 'Patient': 'Member', 'Instrument': 'Instrument']).
fnpattern(circumcise, 45040000, 'Rite', ['Agent': 'Guardian', 'Patient': 'Object', 'Instrument': 'Instrument']).
fnpattern(clear, 45040000, 'Emptying', ['Agent': 'Agent', 'Instrument': 'Theme', 'Patient': 'Source']).
fnpattern(clear, 45040000, 'Emptying', ['Agent': 'Cause', 'Instrument': 'Theme', 'Patient': 'Source']).
fnpattern(coagulate, 45040000, 'Change_of_consistency', ['Patient': 'Undergoer']).
fnpattern(conciliate, 45040000, 'Experiencer_obj', ['Agent': 'Stimulus', 'Patient': 'Experiencer', 'Instrument': 'Means']).
fnpattern(condense, 45040000, 'Change_of_phase', ['Patient': 'Undergoer']).
fnpattern(contract, 45040000, 'Expansion', ['Patient': 'Item']).
fnpattern(cool, 45040000, 'Cause_temperature_change', ['Agent': 'Cause', 'Patient': 'Item', 'Instrument': 'Hot_Cold_source']).
fnpattern(cool, 45040000, 'Cause_temperature_change', ['Agent': 'Agent', 'Patient': 'Item', 'Instrument': 'Hot_Cold_source']).
fnpattern(cool, 45040000, 'Inchoative_change_of_temperature', ['Patient': 'Item']).
fnpattern(corrode, 45040000, 'Corroding', ['Patient': 'Undergoer']).
fnpattern(curdle, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(curdle, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Change_agent']).
fnpattern(curdle, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Means']).
fnpattern(curdle, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(curdle, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Change_agent']).
fnpattern(curdle, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Means']).
fnpattern(curdle, 45040000, 'Change_of_consistency', ['Patient': 'Undergoer']).
fnpattern(dampen, 45040000, 'Cause_to_be_wet', ['Agent': 'Agent', 'Instrument': 'Liquid', 'Patient': 'Undergoer']).
fnpattern(dampen, 45040000, 'Cause_to_be_wet', ['Agent': 'Cause', 'Instrument': 'Liquid', 'Patient': 'Undergoer']).
fnpattern(decompose, 45040000, 'Rotting', ['Patient': 'Undergoer']).
fnpattern(decrease, 45040000, 'Cause_change_of_scalar_position', ['Agent': 'Agent', 'Patient': 'Attribute']).
fnpattern(decrease, 45040000, 'Cause_change_of_scalar_position', ['Agent': 'Cause', 'Patient': 'Attribute']).
fnpattern(decrease, 45040000, 'Change_position_on_a_scale', ['Patient': 'Attribute']).
fnpattern(defrost, 45040000, 'Cause_change_of_phase', ['Agent': 'Agent', 'Patient': 'Undergoer']).
fnpattern(defrost, 45040000, 'Cause_change_of_phase', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(defrost, 45040000, 'Change_of_phase', ['Patient': 'Undergoer']).
fnpattern(dehumidify, 45040000, 'Cause_to_be_dry', ['Agent': 'Agent', 'Instrument': 'Instrument', 'Patient': 'Dryee']).
fnpattern(dehumidify, 45040000, 'Cause_to_be_dry', ['Agent': 'Cause', 'Instrument': 'Instrument', 'Patient': 'Dryee']).
fnpattern(dehydrate, 45040000, 'Cause_to_be_dry', ['Agent': 'Agent', 'Instrument': 'Instrument', 'Patient': 'Dryee']).
fnpattern(dehydrate, 45040000, 'Cause_to_be_dry', ['Agent': 'Cause', 'Instrument': 'Instrument', 'Patient': 'Dryee']).
fnpattern(desiccate, 45040000, 'Cause_to_be_dry', ['Agent': 'Agent', 'Instrument': 'Instrument', 'Patient': 'Dryee']).
fnpattern(desiccate, 45040000, 'Cause_to_be_dry', ['Agent': 'Cause', 'Instrument': 'Instrument', 'Patient': 'Dryee']).
fnpattern(diminish, 45040000, 'Change_position_on_a_scale', ['Patient': 'Attribute']).
fnpattern(disperse, 45040000, 'Dispersal', ['Agent': 'Agent', 'Patient': 'Individuals', 'Instrument': 'Means']).
fnpattern(disperse, 45040000, 'Dispersal', ['Agent': 'Cause', 'Patient': 'Individuals', 'Instrument': 'Means']).
fnpattern(divide, 45040000, 'Separation', ['Agent': 'Agent', 'Patient': 'Whole', 'Instrument': 'Instrument']).
fnpattern(divide, 45040000, 'Separation', ['Agent': 'Cause', 'Patient': 'Whole', 'Instrument': 'Instrument']).
fnpattern(double, 45040000, 'Change_position_on_a_scale', ['Patient': 'Attribute']).
fnpattern(drain, 45040000, 'Emptying', ['Agent': 'Agent', 'Instrument': 'Theme', 'Patient': 'Source']).
fnpattern(drain, 45040000, 'Emptying', ['Agent': 'Cause', 'Instrument': 'Theme', 'Patient': 'Source']).
fnpattern(dry, 45040000, 'Cause_to_be_dry', ['Agent': 'Agent', 'Instrument': 'Instrument', 'Patient': 'Dryee']).
fnpattern(dry, 45040000, 'Cause_to_be_dry', ['Agent': 'Cause', 'Instrument': 'Instrument', 'Patient': 'Dryee']).
fnpattern(ease, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Treatment', 'Patient': 'Affliction']).
fnpattern(ease, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Treatment', 'Patient': 'Patient']).
fnpattern(ease, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Treatment', 'Patient': 'Body_part']).
fnpattern(ease, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Medication', 'Patient': 'Affliction']).
fnpattern(ease, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Medication', 'Patient': 'Patient']).
fnpattern(ease, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Medication', 'Patient': 'Body_part']).
fnpattern(embalm, 45040000, 'Preserving', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Medium']).
fnpattern(embitter, 45040000, 'Experiencer_obj', ['Agent': 'Stimulus', 'Patient': 'Experiencer', 'Instrument': 'Means']).
fnpattern(empty, 45040000, 'Emptying', ['Agent': 'Agent', 'Instrument': 'Theme', 'Patient': 'Source']).
fnpattern(empty, 45040000, 'Emptying', ['Agent': 'Cause', 'Instrument': 'Theme', 'Patient': 'Source']).
fnpattern(enlarge, 45040000, 'Cause_expansion', ['Agent': 'Agent', 'Patient': 'Item']).
fnpattern(enlarge, 45040000, 'Cause_expansion', ['Agent': 'Cause', 'Patient': 'Item']).
fnpattern(enlarge, 45040000, 'Expansion', ['Patient': 'Item']).
fnpattern(evaporate, 45040000, 'Cause_change_of_phase', ['Agent': 'Agent', 'Patient': 'Undergoer']).
fnpattern(evaporate, 45040000, 'Cause_change_of_phase', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(evaporate, 45040000, 'Change_of_phase', ['Patient': 'Undergoer']).
fnpattern(expand, 45040000, 'Expansion', ['Patient': 'Item']).
fnpattern(explode, 45040000, 'Change_position_on_a_scale', ['Patient': 'Attribute']).
fnpattern(fill, 45040000, 'Filling', ['Agent': 'Agent', 'Patient': 'Goal', 'Instrument': 'Theme']).
fnpattern(flatten, 45040000, 'Reshaping', ['Agent': 'Deformer', 'Instrument': 'Instrument', 'Patient': 'Undergoer']).
fnpattern(flatten, 45040000, 'Reshaping', ['Agent': 'Cause', 'Instrument': 'Instrument', 'Patient': 'Undergoer']).
fnpattern(flood, 45040000, 'Filling', ['Agent': 'Agent', 'Patient': 'Goal', 'Instrument': 'Theme']).
fnpattern(freeze, 45040000, 'Cause_change_of_phase', ['Agent': 'Agent', 'Patient': 'Undergoer']).
fnpattern(freeze, 45040000, 'Cause_change_of_phase', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(freeze, 45040000, 'Change_of_phase', ['Patient': 'Undergoer']).
fnpattern(fuse, 45040000, 'Amalgamation', ['Patient': 'Parts']).
fnpattern(fuse, 45040000, 'Cause_to_amalgamate', ['Agent': 'Agent', 'Patient': 'Parts']).
fnpattern(grow, 45040000, 'Change_position_on_a_scale', ['Patient': 'Attribute']).
fnpattern(harden, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(harden, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Change_agent']).
fnpattern(harden, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Means']).
fnpattern(harden, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(harden, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Change_agent']).
fnpattern(harden, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Means']).
fnpattern(harden, 45040000, 'Change_of_consistency', ['Patient': 'Undergoer']).
fnpattern(heal, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Treatment', 'Patient': 'Affliction']).
fnpattern(heal, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Treatment', 'Patient': 'Patient']).
fnpattern(heal, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Treatment', 'Patient': 'Body_part']).
fnpattern(heal, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Medication', 'Patient': 'Affliction']).
fnpattern(heal, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Medication', 'Patient': 'Patient']).
fnpattern(heal, 45040000, 'Cure', ['Agent': 'Healer', 'Instrument': 'Medication', 'Patient': 'Body_part']).
fnpattern(heat, 45040000, 'Cause_temperature_change', ['Agent': 'Cause', 'Patient': 'Item', 'Instrument': 'Hot_Cold_source']).
fnpattern(heat, 45040000, 'Cause_temperature_change', ['Agent': 'Agent', 'Patient': 'Item', 'Instrument': 'Hot_Cold_source']).
fnpattern(heat, 45040000, 'Inchoative_change_of_temperature', ['Patient': 'Item']).
fnpattern(humidify, 45040000, 'Cause_to_be_wet', ['Agent': 'Agent', 'Instrument': 'Liquid', 'Patient': 'Undergoer']).
fnpattern(humidify, 45040000, 'Cause_to_be_wet', ['Agent': 'Cause', 'Instrument': 'Liquid', 'Patient': 'Undergoer']).
fnpattern(hush, 45040000, 'Become_silent', ['Patient': 'Expressor']).
fnpattern(hush, 45040000, 'Become_silent', ['Patient': 'Speaker']).
fnpattern(hush, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Speaker', 'Instrument': 'Instrument']).
fnpattern(hush, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Speaker', 'Instrument': 'Means']).
fnpattern(hush, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Expressor', 'Instrument': 'Instrument']).
fnpattern(hush, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Expressor', 'Instrument': 'Means']).
fnpattern(ignite, 45040000, 'Setting_fire', ['Agent': 'Kindler', 'Patients': 'Flame', 'Patient': 'Flammables']).
fnpattern(ignite, 45040000, 'Setting_fire', ['Agent': 'Cause', 'Patients': 'Flame', 'Patient': 'Flammables']).
fnpattern(increase, 45040000, 'Cause_change_of_scalar_position', ['Agent': 'Agent', 'Patient': 'Attribute']).
fnpattern(increase, 45040000, 'Cause_change_of_scalar_position', ['Agent': 'Cause', 'Patient': 'Attribute']).
fnpattern(increase, 45040000, 'Change_position_on_a_scale', ['Patient': 'Attribute']).
fnpattern(inflate, 45040000, 'Cause_expansion', ['Agent': 'Agent', 'Patient': 'Item']).
fnpattern(inflate, 45040000, 'Cause_expansion', ['Agent': 'Cause', 'Patient': 'Item']).
fnpattern(kindle, 45040000, 'Setting_fire', ['Agent': 'Kindler', 'Patients': 'Flame', 'Patient': 'Flammables']).
fnpattern(kindle, 45040000, 'Setting_fire', ['Agent': 'Cause', 'Patients': 'Flame', 'Patient': 'Flammables']).
fnpattern(lengthen, 45040000, 'Cause_expansion', ['Agent': 'Agent', 'Patient': 'Item']).
fnpattern(lengthen, 45040000, 'Cause_expansion', ['Agent': 'Cause', 'Patient': 'Item']).
fnpattern(light, 45040000, 'Setting_fire', ['Agent': 'Kindler', 'Patients': 'Flame', 'Patient': 'Flammables']).
fnpattern(light, 45040000, 'Setting_fire', ['Agent': 'Cause', 'Patients': 'Flame', 'Patient': 'Flammables']).
fnpattern(liquefy, 45040000, 'Cause_change_of_phase', ['Agent': 'Agent', 'Patient': 'Undergoer']).
fnpattern(liquefy, 45040000, 'Cause_change_of_phase', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(liquefy, 45040000, 'Change_of_phase', ['Patient': 'Undergoer']).
fnpattern(magnify, 45040000, 'Cause_expansion', ['Agent': 'Agent', 'Patient': 'Item']).
fnpattern(magnify, 45040000, 'Cause_expansion', ['Agent': 'Cause', 'Patient': 'Item']).
fnpattern(melt, 45040000, 'Cause_change_of_phase', ['Agent': 'Agent', 'Patient': 'Undergoer']).
fnpattern(melt, 45040000, 'Cause_change_of_phase', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(melt, 45040000, 'Change_of_phase', ['Patient': 'Undergoer']).
fnpattern(moisten, 45040000, 'Cause_to_be_wet', ['Agent': 'Agent', 'Instrument': 'Liquid', 'Patient': 'Undergoer']).
fnpattern(moisten, 45040000, 'Cause_to_be_wet', ['Agent': 'Cause', 'Instrument': 'Liquid', 'Patient': 'Undergoer']).
fnpattern(narrow, 45040000, 'Cause_expansion', ['Agent': 'Agent', 'Patient': 'Item']).
fnpattern(narrow, 45040000, 'Cause_expansion', ['Agent': 'Cause', 'Patient': 'Item']).
fnpattern(obscure, 45040000, 'Eclipse', ['Agent': 'Obstruction', 'Patient': 'Eclipsed']).
fnpattern(oxidize, 45040000, 'Corroding_caused', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(putrefy, 45040000, 'Rotting', ['Patient': 'Undergoer']).
fnpattern(quiet, 45040000, 'Become_silent', ['Patient': 'Expressor']).
fnpattern(quiet, 45040000, 'Become_silent', ['Patient': 'Speaker']).
fnpattern(quiet, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Speaker', 'Instrument': 'Instrument']).
fnpattern(quiet, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Speaker', 'Instrument': 'Means']).
fnpattern(quiet, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Expressor', 'Instrument': 'Instrument']).
fnpattern(quiet, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Expressor', 'Instrument': 'Means']).
fnpattern(reproduce, 45040000, 'Duplication', ['Agent': 'Copy', 'Patient': 'Original']).
fnpattern(reproduce, 45040000, 'Duplication', ['Agent': 'Creator', 'Patient': 'Original']).
fnpattern(shrink, 45040000, 'Cause_expansion', ['Agent': 'Agent', 'Patient': 'Item']).
fnpattern(shrink, 45040000, 'Cause_expansion', ['Agent': 'Cause', 'Patient': 'Item']).
fnpattern(shrink, 45040000, 'Expansion', ['Patient': 'Item']).
fnpattern(silence, 45040000, 'Become_silent', ['Patient': 'Expressor']).
fnpattern(silence, 45040000, 'Become_silent', ['Patient': 'Speaker']).
fnpattern(silence, 45040000, 'Killing', ['Agent': 'Killer', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(silence, 45040000, 'Killing', ['Agent': 'Cause', 'Patient': 'Victim', 'Instrument': 'Instrument']).
fnpattern(silence, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Speaker', 'Instrument': 'Instrument']).
fnpattern(silence, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Speaker', 'Instrument': 'Means']).
fnpattern(silence, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Expressor', 'Instrument': 'Instrument']).
fnpattern(silence, 45040000, 'Silencing', ['Agent': 'Agent', 'Patient': 'Expressor', 'Instrument': 'Means']).
fnpattern(soak, 45040000, 'Cause_to_be_wet', ['Agent': 'Agent', 'Instrument': 'Liquid', 'Patient': 'Undergoer']).
fnpattern(soak, 45040000, 'Cause_to_be_wet', ['Agent': 'Cause', 'Instrument': 'Liquid', 'Patient': 'Undergoer']).
fnpattern(soften, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(soften, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Change_agent']).
fnpattern(soften, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Means']).
fnpattern(soften, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(soften, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Change_agent']).
fnpattern(soften, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Means']).
fnpattern(soften, 45040000, 'Change_of_consistency', ['Patient': 'Undergoer']).
fnpattern(solidify, 45040000, 'Change_of_phase', ['Patient': 'Undergoer']).
fnpattern(stretch, 45040000, 'Expansion', ['Patient': 'Item']).
fnpattern(thaw, 45040000, 'Cause_change_of_phase', ['Agent': 'Agent', 'Patient': 'Undergoer']).
fnpattern(thaw, 45040000, 'Cause_change_of_phase', ['Agent': 'Cause', 'Patient': 'Undergoer']).
fnpattern(thaw, 45040000, 'Change_of_phase', ['Patient': 'Undergoer']).
fnpattern(thicken, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(thicken, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Change_agent']).
fnpattern(thicken, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Means']).
fnpattern(thicken, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(thicken, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Change_agent']).
fnpattern(thicken, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Means']).
fnpattern(thicken, 45040000, 'Change_of_consistency', ['Patient': 'Undergoer']).
fnpattern(thin, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(thin, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Change_agent']).
fnpattern(thin, 45040000, 'Cause_change_of_consistency', ['Agent': 'Agent', 'Patient': 'Undergoer', 'Instrument': 'Means']).
fnpattern(thin, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Instrument']).
fnpattern(thin, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Change_agent']).
fnpattern(thin, 45040000, 'Cause_change_of_consistency', ['Agent': 'Cause', 'Patient': 'Undergoer', 'Instrument': 'Means']).
fnpattern(triple, 45040000, 'Change_position_on_a_scale', ['Patient': 'Attribute']).
fnpattern(vaporize, 45040000, 'Change_of_phase', ['Patient': 'Undergoer']).
fnpattern(wake, 45040000, 'Cause_to_wake', ['Agent': 'Agent', 'Patient': 'Sleeper', 'Instrument': 'Instrument']).
fnpattern(wake, 45040000, 'Cause_to_wake', ['Agent': 'Cause', 'Patient': 'Sleeper', 'Instrument': 'Instrument']).
fnpattern(wake, 45040000, 'Waking_up', ['Patient': 'Sleeper']).
fnpattern(warm, 45040000, 'Cause_temperature_change', ['Agent': 'Cause', 'Patient': 'Item', 'Instrument': 'Hot_Cold_source']).
fnpattern(warm, 45040000, 'Cause_temperature_change', ['Agent': 'Agent', 'Patient': 'Item', 'Instrument': 'Hot_Cold_source']).
fnpattern(warm, 45040000, 'Inchoative_change_of_temperature', ['Patient': 'Item']).
fnpattern(warp, 45040000, 'Reshaping', ['Agent': 'Deformer', 'Instrument': 'Instrument', 'Patient': 'Undergoer']).
fnpattern(warp, 45040000, 'Reshaping', ['Agent': 'Cause', 'Instrument': 'Instrument', 'Patient': 'Undergoer']).
fnpattern(wet, 45040000, 'Cause_to_be_wet', ['Agent': 'Agent', 'Instrument': 'Liquid', 'Patient': 'Undergoer']).
fnpattern(wet, 45040000, 'Cause_to_be_wet', ['Agent': 'Cause', 'Instrument': 'Liquid', 'Patient': 'Undergoer']).
fnpattern(widen, 45040000, 'Cause_expansion', ['Agent': 'Agent', 'Patient': 'Item']).
fnpattern(widen, 45040000, 'Cause_expansion', ['Agent': 'Cause', 'Patient': 'Item']).
fnpattern(corrode, 45050000, 'Corroding', ['Patient': 'Undergoer']).
fnpattern(corrode, 45050000, 'Corroding_caused', ['Patient': 'Undergoer']).
fnpattern(decay, 45050000, 'Rotting', ['Patient': 'Undergoer']).
fnpattern(rot, 45050000, 'Rotting', ['Patient': 'Undergoer']).
fnpattern(rust, 45050000, 'Corroding', ['Patient': 'Undergoer']).
fnpattern(spoil, 45050000, 'Rotting', ['Patient': 'Undergoer']).
fnpattern(swell, 45050000, 'Cause_change_of_scalar_position', ['Patient': 'Attribute']).
fnpattern(swell, 45050000, 'Cause_expansion', ['Patient': 'Item']).
fnpattern(tarnish, 45050000, 'Corroding_caused', ['Patient': 'Undergoer']).
fnpattern(decline, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(decrease, 45060100, 'Cause_change_of_scalar_position', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(decrease, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(diminish, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(drop, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(fall, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(fluctuate, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(gain, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(grow, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(increase, 45060100, 'Cause_change_of_scalar_position', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(increase, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(jump, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(lower, 45060100, 'Cause_change_of_scalar_position', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(plummet, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(rise, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(rocket, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(soar, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(swell, 45060100, 'Cause_change_of_scalar_position', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(swell, 45060100, 'Cause_expansion', ['Patient': 'Item', 'Attribute': 'Dimension', 'Extent': 'Size_change']).
fnpattern(tumble, 45060100, 'Change_position_on_a_scale', ['Patient': 'Item', 'Attribute': 'Attribute', 'Extent': 'Difference']).
fnpattern(camp, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Goal']).
fnpattern(camp, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Location']).
fnpattern(camp, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Goal']).
fnpattern(camp, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Location']).
fnpattern(dwell, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Goal']).
fnpattern(dwell, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Location']).
fnpattern(dwell, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Goal']).
fnpattern(dwell, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Location']).
fnpattern(live, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Goal']).
fnpattern(live, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Location']).
fnpattern(live, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Goal']).
fnpattern(live, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Location']).
fnpattern(lodge, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Goal']).
fnpattern(lodge, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Location']).
fnpattern(lodge, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Goal']).
fnpattern(lodge, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Location']).
fnpattern(reside, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Goal']).
fnpattern(reside, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Location']).
fnpattern(reside, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Goal']).
fnpattern(reside, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Location']).
fnpattern(stay, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Goal']).
fnpattern(stay, 46000000, 'Residence', ['Theme': 'Resident', 'Location': 'Location']).
fnpattern(stay, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Goal']).
fnpattern(stay, 46000000, 'Residence', ['Theme': 'Co-resident', 'Location': 'Location']).
fnpattern(exist, 47010100, 'Existence', ['Theme': 'Entity']).
fnpattern(persist, 47010100, 'Process_continue', ['Theme': 'Event', 'Location': 'Goal']).
fnpattern(persist, 47010100, 'Process_continue', ['Theme': 'Event', 'Location': 'Place']).
fnpattern(cascade, 47020000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Goal']).
fnpattern(cascade, 47020000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Area']).
fnpattern(corrode, 47020000, 'Corroding', ['Theme': 'Undergoer']).
fnpattern(decay, 47020000, 'Rotting', ['Theme': 'Undergoer']).
fnpattern(decompose, 47020000, 'Rotting', ['Theme': 'Undergoer']).
fnpattern(fester, 47020000, 'Rotting', ['Theme': 'Undergoer']).
fnpattern(fizz, 47020000, 'Motion_noise', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(fizz, 47020000, 'Motion_noise', ['Location': 'Goal', 'Theme': 'Theme']).
fnpattern(flow, 47020000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Goal']).
fnpattern(flow, 47020000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Area']).
fnpattern(rot, 47020000, 'Rotting', ['Theme': 'Undergoer']).
fnpattern(rust, 47020000, 'Corroding', ['Theme': 'Undergoer']).
fnpattern(spread, 47020000, 'Dispersal', ['Theme': 'Individuals', 'Location': 'Source']).
fnpattern(spread, 47020000, 'Dispersal', ['Theme': 'Individuals', 'Location': 'Goal_area']).
fnpattern(stream, 47020000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Goal']).
fnpattern(stream, 47020000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Area']).
fnpattern(tarnish, 47020000, 'Corroding_caused', ['Theme': 'Undergoer']).
fnpattern(trickle, 47020000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Goal']).
fnpattern(trickle, 47020000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Area']).
fnpattern(bob, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Goal']).
fnpattern(bob, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Area']).
fnpattern(creep, 47030000, 'Self_motion', ['Location': 'Area', 'Theme': 'Self_mover']).
fnpattern(creep, 47030000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(dance, 47030000, 'Self_motion', ['Location': 'Area', 'Theme': 'Self_mover']).
fnpattern(dance, 47030000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(drift, 47030000, 'Motion', ['Location': 'Area', 'Theme': 'Self_mover']).
fnpattern(drift, 47030000, 'Motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(flap, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Goal']).
fnpattern(flap, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Area']).
fnpattern(float, 47030000, 'Motion', ['Location': 'Area', 'Theme': 'Self_mover']).
fnpattern(float, 47030000, 'Motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(flutter, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Goal']).
fnpattern(flutter, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Area']).
fnpattern(jiggle, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Goal']).
fnpattern(jiggle, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Area']).
fnpattern(rotate, 47030000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(rotate, 47030000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(rotate, 47030000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(rotate, 47030000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(rotate, 47030000, 'Moving_in_place', ['Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(shake, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Goal']).
fnpattern(shake, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Area']).
fnpattern(shake, 47030000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(shake, 47030000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(shake, 47030000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(shake, 47030000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(shake, 47030000, 'Moving_in_place', ['Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(totter, 47030000, 'Self_motion', ['Location': 'Area', 'Theme': 'Self_mover']).
fnpattern(totter, 47030000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(vibrate, 47030000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(vibrate, 47030000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(vibrate, 47030000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(vibrate, 47030000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(vibrate, 47030000, 'Moving_in_place', ['Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(wave, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Goal']).
fnpattern(wave, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Area']).
fnpattern(wiggle, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Goal']).
fnpattern(wiggle, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Area']).
fnpattern(writhe, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Goal']).
fnpattern(writhe, 47030000, 'Body_movement', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Area']).
fnpattern(echo, 47040000, 'Sound_movement', ['Theme': 'Sound', 'Location': 'Sound_source']).
fnpattern(resound, 47040000, 'Sound_movement', ['Theme': 'Sound', 'Location': 'Sound_source']).
fnpattern(reverberate, 47040000, 'Sound_movement', ['Theme': 'Sound', 'Location': 'Sound_source']).
fnpattern(bustle, 47051100, 'Self_motion', ['Location': 'Goal', 'Theme': 'Self_mover']).
fnpattern(bustle, 47051100, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(crawl, 47051100, 'Abundance', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(crawl, 47051100, 'Self_motion', ['Location': 'Goal', 'Theme': 'Self_mover']).
fnpattern(crawl, 47051100, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(creep, 47051100, 'Self_motion', ['Location': 'Goal', 'Theme': 'Self_mover']).
fnpattern(creep, 47051100, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(run, 47051100, 'Self_motion', ['Location': 'Goal', 'Theme': 'Self_mover']).
fnpattern(run, 47051100, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(swarm, 47051100, 'Abundance', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(swarm, 47051100, 'Mass_motion', ['Theme': 'Mass_theme', 'Location': 'Goal']).
fnpattern(swarm, 47051100, 'Mass_motion', ['Theme': 'Mass_theme', 'Location': 'Location']).
fnpattern(swim, 47051100, 'Self_motion', ['Location': 'Goal', 'Theme': 'Self_mover']).
fnpattern(swim, 47051100, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(teem, 47051100, 'Abundance', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(teem, 47051100, 'Mass_motion', ['Theme': 'Mass_theme', 'Location': 'Goal']).
fnpattern(teem, 47051100, 'Mass_motion', ['Theme': 'Mass_theme', 'Location': 'Location']).
fnpattern(throng, 47051100, 'Abundance', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(throng, 47051200, 'Mass_motion', ['Theme': 'Mass_theme', 'Location': 'Goal']).
fnpattern(throng, 47051200, 'Mass_motion', ['Theme': 'Mass_theme', 'Location': 'Location']).
fnpattern(assemble, 47052000, 'Congregating', ['Theme': 'Individuals']).
fnpattern(assemble, 47052000, 'Gathering_up', ['Agent': 'Agent', 'Theme': 'Individuals']).
fnpattern(collect, 47052000, 'Commerce_collect', ['Agent': 'Buyer', 'Theme': 'Goods']).
fnpattern(congregate, 47052000, 'Congregating', ['Theme': 'Individuals']).
fnpattern(convene, 47052000, 'Congregating', ['Theme': 'Individuals']).
fnpattern(convene, 47052000, 'Gathering_up', ['Agent': 'Agent', 'Theme': 'Individuals']).
fnpattern(flock, 47052000, 'Mass_motion', ['Theme': 'Mass_theme']).
fnpattern(gather, 47052000, 'Congregating', ['Theme': 'Individuals']).
fnpattern(gather, 47052000, 'Gathering_up', ['Agent': 'Agent', 'Theme': 'Individuals']).
fnpattern(herd, 47052000, 'Gathering_up', ['Agent': 'Agent', 'Theme': 'Individuals']).
fnpattern(bend, 47060000, 'Posture', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(crouch, 47060000, 'Posture', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(kneel, 47060000, 'Posture', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(lean, 47060000, 'Posture', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(lie, 47060000, 'Posture', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(sit, 47060000, 'Posture', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(slouch, 47060000, 'Posture', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(sprawl, 47060000, 'Posture', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(squat, 47060000, 'Posture', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(stand, 47060000, 'Posture', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(stoop, 47060000, 'Posture', ['Theme': 'Theme', 'Location': 'Location']).
fnpattern(cascade, 47070000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Goal']).
fnpattern(cascade, 47070000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Area']).
fnpattern(climb, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(climb, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(crawl, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(crawl, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(drop, 47070000, 'Motion_directional', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(drop, 47070000, 'Motion_directional', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(drop, 47070000, 'Motion_directional', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(drop, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Path']).
fnpattern(drop, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Area']).
fnpattern(go, 47070000, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(go, 47070000, 'Motion', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(meander, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Path']).
fnpattern(meander, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Area']).
fnpattern(meander, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(meander, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(plunge, 47070000, 'Motion_directional', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(plunge, 47070000, 'Motion_directional', ['Theme': 'Theme', 'Location': 'Area']).
fnpattern(plunge, 47070000, 'Motion_directional', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(run, 47070000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Goal']).
fnpattern(run, 47070000, 'Fluidic_motion', ['Theme': 'Fluid', 'Location': 'Area']).
fnpattern(run, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(run, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(straggle, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(straggle, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(stretch, 47070000, 'Expansion', ['Theme': 'Item']).
fnpattern(swerve, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Path']).
fnpattern(swerve, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Area']).
fnpattern(veer, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Path']).
fnpattern(veer, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Area']).
fnpattern(wander, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(wander, 47070000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(weave, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Path']).
fnpattern(weave, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Area']).
fnpattern(wind, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Path']).
fnpattern(wind, 47070000, 'Path_shape', ['Theme': 'Road', 'Location': 'Area']).
fnpattern(blanket, 47080000, 'Adorning', ['Theme1': 'Theme', 'Theme2': 'Location']).
fnpattern(cap, 47080000, 'Closure', ['Theme1': 'Fastener', 'Theme2': 'Containing_object']).
fnpattern(contain, 47080000, 'Containing', ['Theme1': 'Container', 'Theme2': 'Contents']).
fnpattern(cover, 47080000, 'Adorning', ['Theme1': 'Theme', 'Theme2': 'Location']).
fnpattern(encircle, 47080000, 'Adorning', ['Theme1': 'Theme', 'Theme2': 'Location']).
fnpattern(fill, 47080000, 'Adorning', ['Theme1': 'Theme', 'Theme2': 'Location']).
fnpattern(follow, 47080000, 'Cotheme', ['Theme1': 'Theme', 'Theme2': 'Cotheme']).
fnpattern(line, 47080000, 'Adorning', ['Theme1': 'Theme', 'Theme2': 'Location']).
fnpattern(traverse, 47080000, 'Path_shape', ['Theme1': 'Theme', 'Theme2': 'Road']).
fnpattern(appear, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Place']).
fnpattern(appear, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Time']).
fnpattern(arise, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Place']).
fnpattern(arise, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Time']).
fnpattern(develop, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Place']).
fnpattern(develop, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Time']).
fnpattern(emerge, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Place']).
fnpattern(emerge, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Time']).
fnpattern(erupt, 48011000, 'Process_start', ['Theme': 'Event', 'Location': 'Place']).
fnpattern(erupt, 48011000, 'Process_start', ['Theme': 'Event', 'Location': 'Time']).
fnpattern(form, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Place']).
fnpattern(form, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Time']).
fnpattern(materialize, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Place']).
fnpattern(materialize, 48011000, 'Coming_to_be', ['Theme': 'Entity', 'Location': 'Time']).
fnpattern(assert, 48012000, 'Statement', ['Theme': 'Topic', 'Agent': 'Speaker', 'Recipient': 'Addressee']).
fnpattern(assert, 48012000, 'Statement', ['Theme': 'Message', 'Agent': 'Speaker', 'Recipient': 'Addressee']).
fnpattern(declare, 48012000, 'Statement', ['Theme': 'Topic', 'Agent': 'Speaker', 'Recipient': 'Addressee']).
fnpattern(declare, 48012000, 'Statement', ['Theme': 'Message', 'Agent': 'Speaker', 'Recipient': 'Addressee']).
fnpattern(expose, 48012000, 'Reveal_secret', ['Theme': 'Topic', 'Agent': 'Speaker', 'Recipient': 'Addressee']).
fnpattern(expose, 48012000, 'Reveal_secret', ['Theme': 'Information', 'Agent': 'Speaker', 'Recipient': 'Addressee']).
fnpattern(express, 48012000, 'Statement', ['Theme': 'Topic', 'Agent': 'Speaker', 'Recipient': 'Addressee']).
fnpattern(express, 48012000, 'Statement', ['Theme': 'Message', 'Agent': 'Speaker', 'Recipient': 'Addressee']).
fnpattern(suggest, 48012000, 'Statement', ['Theme': 'Topic', 'Agent': 'Speaker', 'Recipient': 'Addressee']).
fnpattern(suggest, 48012000, 'Statement', ['Theme': 'Message', 'Agent': 'Speaker', 'Recipient': 'Addressee']).
fnpattern(die, 48020000, 'Death', ['Theme': 'Protagonist']).
fnpattern(disappear, 48020000, 'Departing', ['Theme': 'Theme', 'Location': 'Source']).
fnpattern(expire, 48020000, 'Death', ['Theme': 'Protagonist']).
fnpattern(perish, 48020000, 'Death', ['Theme': 'Protagonist']).
fnpattern(vanish, 48020000, 'Departing', ['Theme': 'Theme', 'Location': 'Source']).
fnpattern(befall, 48030000, 'Catastrophe', ['Theme': 'Undesirable_event', 'Location': 'Place']).
fnpattern(befall, 48030000, 'Catastrophe', ['Theme': 'Undesirable_event', 'Location': 'Time']).
fnpattern(chance, 48030000, 'Daring', ['Theme': 'Action', 'Location': 'Place']).
fnpattern(chance, 48030000, 'Daring', ['Theme': 'Action', 'Location': 'Time']).
fnpattern(happen, 48030000, 'Event', ['Theme': 'Event', 'Location': 'Place']).
fnpattern(happen, 48030000, 'Event', ['Theme': 'Event', 'Location': 'Time']).
fnpattern(occur, 48030000, 'Event', ['Theme': 'Event', 'Location': 'Place']).
fnpattern(occur, 48030000, 'Event', ['Theme': 'Event', 'Location': 'Time']).
fnpattern(fidget, 49000000, 'Body_movement', ['Agent': 'Entity', 'Patient': 'Body_part']).
fnpattern(flap, 49000000, 'Body_movement', ['Agent': 'Entity', 'Patient': 'Body_part']).
fnpattern(totter, 49000000, 'Self_motion', ['Patient': 'Self_mover']).
fnpattern(twitch, 49000000, 'Body_movement', ['Agent': 'Entity', 'Patient': 'Body_part']).
fnpattern(waggle, 49000000, 'Body_movement', ['Agent': 'Entity', 'Patient': 'Body_part']).
fnpattern(wiggle, 49000000, 'Body_movement', ['Agent': 'Entity', 'Patient': 'Body_part']).
fnpattern(wriggle, 49000000, 'Body_movement', ['Agent': 'Entity', 'Patient': 'Body_part']).
fnpattern(wriggle, 49000000, 'Self_motion', ['Patient': 'Self_mover']).
fnpattern(bend, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(crouch, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(kneel, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(lean, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(lie, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(rise, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(sit, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(slouch, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(sprawl, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(squat, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(stand, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(stoop, 50000000, 'Change_posture', ['Agent': 'Protagonist']).
fnpattern(ascend, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(ascend, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Road']).
fnpattern(climb, 51010100, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(climb, 51010100, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(cross, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(cross, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Road']).
fnpattern(depart, 51010100, 'Departing', ['Theme': 'Theme', 'Location': 'Source']).
fnpattern(depart, 51010100, 'Departing', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(descend, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(descend, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Road']).
fnpattern(enter, 51010100, 'Arriving', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(enter, 51010100, 'Arriving', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(enter, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(enter, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Road']).
fnpattern(escape, 51010100, 'Departing', ['Theme': 'Theme', 'Location': 'Source']).
fnpattern(escape, 51010100, 'Departing', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(escape, 51010100, 'Escaping', ['Theme': 'Theme', 'Location': 'Undesirable_location']).
fnpattern(exit, 51010100, 'Departing', ['Theme': 'Theme', 'Location': 'Source']).
fnpattern(exit, 51010100, 'Departing', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(exit, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(exit, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Road']).
fnpattern(flee, 51010100, 'Escaping', ['Theme': 'Theme', 'Location': 'Undesirable_location']).
fnpattern(flee, 51010100, 'Evading', ['Theme': 'Evader']).
fnpattern(leave, 51010100, 'Departing', ['Theme': 'Theme', 'Location': 'Source']).
fnpattern(leave, 51010100, 'Departing', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(leave, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(leave, 51010100, 'Path_shape', ['Theme': 'Theme', 'Location': 'Road']).
fnpattern(vacate, 51010100, 'Departing', ['Theme': 'Theme', 'Location': 'Source']).
fnpattern(vacate, 51010100, 'Departing', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(vacate, 51010100, 'Quitting_a_place', ['Theme': 'Theme', 'Location': 'Source']).
fnpattern(vacate, 51010100, 'Quitting_a_place', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(approach, 51010200, 'Arriving', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(approach, 51010200, 'Arriving', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(come, 51010200, 'Arriving', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(come, 51010200, 'Arriving', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(fall, 51010200, 'Motion_directional', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(fall, 51010200, 'Motion_directional', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(go, 51010200, 'Motion', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(go, 51010200, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(plunge, 51010200, 'Motion_directional', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(plunge, 51010200, 'Motion_directional', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(return, 51010200, 'Arriving', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(return, 51010200, 'Arriving', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(rise, 51010200, 'Path_shape', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(rise, 51010200, 'Path_shape', ['Theme': 'Theme', 'Location': 'Road']).
fnpattern(arrive, 51010210, 'Arriving', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(arrive, 51010210, 'Arriving', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(abandon, 51020000, 'Departing', ['Theme': 'Theme', 'Source': 'Source']).
fnpattern(desert, 51020100, 'Departing', ['Theme': 'Theme', 'Source': 'Source']).
fnpattern(leave, 51020100, 'Departing', ['Theme': 'Theme', 'Source': 'Source']).
fnpattern(drift, 51031000, 'Motion', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(drift, 51031000, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(drop, 51031000, 'Motion_directional', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(drop, 51031000, 'Motion_directional', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(drop, 51031000, 'Path_shape', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(drop, 51031000, 'Path_shape', ['Theme': 'Theme', 'Location': 'Road']).
fnpattern(float, 51031000, 'Motion', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(float, 51031000, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(glide, 51031000, 'Motion', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(glide, 51031000, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(move, 51031000, 'Motion', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(move, 51031000, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(roll, 51031000, 'Motion', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(roll, 51031000, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(rotate, 51031000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(rotate, 51031000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(rotate, 51031000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(rotate, 51031000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(rotate, 51031000, 'Moving_in_place', ['Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(spin, 51031000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(spin, 51031000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(spin, 51031000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(spin, 51031000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(swing, 51031000, 'Path_shape', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(swing, 51031000, 'Path_shape', ['Theme': 'Theme', 'Location': 'Road']).
fnpattern(turn, 51031000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(turn, 51031000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(turn, 51031000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(turn, 51031000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(twirl, 51031000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(twirl, 51031000, 'Cause_to_move_in_place', ['Agent': 'Agent', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(twirl, 51031000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(twirl, 51031000, 'Cause_to_move_in_place', ['Agent': 'Cause', 'Theme': 'Body_part', 'Location': 'Fixed_location']).
fnpattern(twirl, 51031000, 'Moving_in_place', ['Theme': 'Theme', 'Location': 'Fixed_location']).
fnpattern(wind, 51031000, 'Path_shape', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(wind, 51031000, 'Path_shape', ['Theme': 'Theme', 'Location': 'Road']).
fnpattern(amble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(amble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(amble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(bolt, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(bolt, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(bolt, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(bound, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(bound, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(bound, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(canter, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(canter, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(canter, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(clamber, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(clamber, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(clamber, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(climb, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(climb, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(climb, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(coast, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(coast, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(coast, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(crawl, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(crawl, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(crawl, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(creep, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(creep, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(creep, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(dart, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(dart, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(dart, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(dash, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(dash, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(dash, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(drift, 51032000, 'Motion', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(drift, 51032000, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(flit, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(flit, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(flit, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(float, 51032000, 'Motion', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(float, 51032000, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(fly, 51032000, 'Motion', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(fly, 51032000, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(frolic, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(frolic, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(frolic, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(gambol, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(gambol, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(gambol, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(glide, 51032000, 'Motion', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(glide, 51032000, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(hasten, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(hasten, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(hasten, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(hike, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(hike, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(hike, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(hobble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(hobble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(hobble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(hurry, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(hurry, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(hurry, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(jog, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(jog, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(jog, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(jump, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(jump, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(jump, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(leap, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(leap, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(leap, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(limp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(limp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(limp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(lope, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(lope, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(lope, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(lumber, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(lumber, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(lumber, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(lurch, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(lurch, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(lurch, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(march, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(march, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(march, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(meander, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(meander, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(meander, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(mince, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(mince, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(mince, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(mosey, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(mosey, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(mosey, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(pad, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(pad, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(pad, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(parade, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(parade, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(parade, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(plod, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(plod, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(plod, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(prance, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(prance, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(prance, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(promenade, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(promenade, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(promenade, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(prowl, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(prowl, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(prowl, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(roam, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(roam, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(roam, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(roll, 51032000, 'Motion', ['Location': 'Area', 'Theme': 'Theme']).
fnpattern(roll, 51032000, 'Motion', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(romp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(romp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(romp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(run, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(run, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(run, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(rush, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(rush, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(rush, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(sashay, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(sashay, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(sashay, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(saunter, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(saunter, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(saunter, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(scamper, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(scamper, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(scamper, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(scoot, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(scoot, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(scoot, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(scramble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(scramble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(scramble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(scurry, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(scurry, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(scurry, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(scuttle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(scuttle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(scuttle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(shuffle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(shuffle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(shuffle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(sidle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(sidle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(sidle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(skip, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(skip, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(skip, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(skulk, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(skulk, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(skulk, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(sleepwalk, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(sleepwalk, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(sleepwalk, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(slink, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(slink, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(slink, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(slither, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(slither, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(slither, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(slog, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(slog, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(slog, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(sneak, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(sneak, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(sneak, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(stagger, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(stagger, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(stagger, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(step, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(step, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(step, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(stomp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(stomp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(stomp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(stride, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(stride, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(stride, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(stroll, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(stroll, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(stroll, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(strut, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(strut, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(strut, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(stumble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(stumble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(stumble, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(swagger, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(swagger, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(swagger, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(swim, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(swim, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(swim, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(tack, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(tack, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(tack, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(tiptoe, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(tiptoe, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(tiptoe, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(toddle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(toddle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(toddle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(totter, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(totter, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(totter, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(tour, 51032000, 'Travel', ['Theme': 'Traveler', 'Location': 'Area']).
fnpattern(tour, 51032000, 'Travel', ['Theme': 'Traveler', 'Location': 'Goal']).
fnpattern(tour, 51032000, 'Travel', ['Theme': 'Traveler', 'Location': 'Path']).
fnpattern(traipse, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(traipse, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(traipse, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(tramp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(tramp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(tramp, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(trek, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(trek, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(trek, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(troop, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(troop, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(troop, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(trot, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(trot, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(trot, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(trudge, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(trudge, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(trudge, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(trundle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(trundle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(trundle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(vault, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(vault, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(vault, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(waddle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(waddle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(waddle, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(wade, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(wade, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(wade, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(walk, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(walk, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(walk, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(wander, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(wander, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Path']).
fnpattern(wander, 51032000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(balloon, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(balloon, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(boat, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(boat, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(caravan, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(caravan, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(jet, 51041000, 'Ride_vehicle', ['Agent': 'Vehicle', 'Theme': 'Theme', 'Location': 'Area']).
fnpattern(jet, 51041000, 'Ride_vehicle', ['Agent': 'Vehicle', 'Theme': 'Theme', 'Location': 'Goal']).
fnpattern(motor, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(motor, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(parachute, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(parachute, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(punt, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(punt, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(sledge, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(sledge, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(taxi, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(taxi, 51041000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(taxi, 51041000, 'Ride_vehicle', ['Agent': 'Vehicle', 'Theme': 'Theme', 'Location': 'Area']).
fnpattern(taxi, 51041000, 'Ride_vehicle', ['Agent': 'Vehicle', 'Theme': 'Theme', 'Location': 'Goal']).
fnpattern(bicycle, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(bicycle, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(bike, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(bike, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(canoe, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(canoe, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(cycle, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(cycle, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(raft, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(raft, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(skate, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(skate, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(toboggan, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(toboggan, 51041100, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(cruise, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(cruise, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(cruise, 51042000, 'Ride_vehicle', ['Agent': 'Vehicle', 'Theme': 'Theme', 'Location': 'Area']).
fnpattern(cruise, 51042000, 'Ride_vehicle', ['Agent': 'Vehicle', 'Theme': 'Theme', 'Location': 'Goal']).
fnpattern(drive, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(drive, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(fly, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(fly, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(fly, 51042000, 'Ride_vehicle', ['Agent': 'Vehicle', 'Theme': 'Theme', 'Location': 'Area']).
fnpattern(fly, 51042000, 'Ride_vehicle', ['Agent': 'Vehicle', 'Theme': 'Theme', 'Location': 'Goal']).
fnpattern(paddle, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(paddle, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(pedal, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(pedal, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(ride, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(ride, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(row, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(row, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(sail, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(sail, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(sail, 51042000, 'Ride_vehicle', ['Agent': 'Vehicle', 'Theme': 'Theme', 'Location': 'Area']).
fnpattern(sail, 51042000, 'Ride_vehicle', ['Agent': 'Vehicle', 'Theme': 'Theme', 'Location': 'Goal']).
fnpattern(tack, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Area']).
fnpattern(tack, 51042000, 'Operate_vehicle', ['Agent': 'Driver', 'Theme': 'Vehicle', 'Location': 'Goal']).
fnpattern(voyage, 51042000, 'Travel', ['Agent': 'Traveler', 'Theme': 'Co-participant', 'Location': 'Area']).
fnpattern(voyage, 51042000, 'Travel', ['Agent': 'Traveler', 'Theme': 'Co-participant', 'Location': 'Goal']).
fnpattern(voyage, 51042000, 'Travel', ['Agent': 'Traveler', 'Theme': 'Co-participant', 'Location': 'Path']).
fnpattern(voyage, 51042000, 'Travel', ['Agent': 'Traveler', 'Theme': 'Baggage', 'Location': 'Area']).
fnpattern(voyage, 51042000, 'Travel', ['Agent': 'Traveler', 'Theme': 'Baggage', 'Location': 'Goal']).
fnpattern(voyage, 51042000, 'Travel', ['Agent': 'Traveler', 'Theme': 'Baggage', 'Location': 'Path']).
fnpattern(bop, 51050000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(bop, 51050000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(dance, 51050000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(dance, 51050000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(shuffle, 51050000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(shuffle, 51050000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(waltz, 51050000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Area']).
fnpattern(waltz, 51050000, 'Self_motion', ['Theme': 'Self_mover', 'Location': 'Goal']).
fnpattern(chase, 51060000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(follow, 51060000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(pursue, 51060000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(shadow, 51060000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(tail, 51060000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(track, 51060000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(trail, 51060000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(accompany, 51070000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(conduct, 51070000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(escort, 51070000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(guide, 51070000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(lead, 51070000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(shepherd, 51070000, 'Cotheme', ['Agent': 'Theme', 'Theme': 'Cotheme']).
fnpattern(reach, 51080000, 'Arriving', ['Theme': 'Theme', 'Location': 'Goal']).
fnpattern(reach, 51080000, 'Arriving', ['Theme': 'Theme', 'Location': 'Path']).
fnpattern(avoid, 52000000, 'Avoiding', ['Agent': 'Agent', 'Theme': 'Undesirable_situation', 'Location': 'Place']).
fnpattern(avoid, 52000000, 'Avoiding', ['Agent': 'Agent', 'Location': 'Undesirable_situation']).
fnpattern(avoid, 52000000, 'Preventing', ['Agent': 'Preventing_cause', 'Theme': 'Event']).
fnpattern(dodge, 52000000, 'Avoiding', ['Agent': 'Agent', 'Theme': 'Undesirable_situation', 'Location': 'Place']).
fnpattern(dodge, 52000000, 'Avoiding', ['Agent': 'Agent', 'Location': 'Undesirable_situation']).
fnpattern(duck, 52000000, 'Avoiding', ['Agent': 'Agent', 'Theme': 'Undesirable_situation', 'Location': 'Place']).
fnpattern(duck, 52000000, 'Avoiding', ['Agent': 'Agent', 'Location': 'Undesirable_situation']).
fnpattern(elude, 52000000, 'Evading', ['Agent': 'Evader', 'Theme': 'Pursuer']).
fnpattern(eschew, 52000000, 'Avoiding', ['Agent': 'Agent', 'Theme': 'Undesirable_situation', 'Location': 'Place']).
fnpattern(eschew, 52000000, 'Avoiding', ['Agent': 'Agent', 'Location': 'Undesirable_situation']).
fnpattern(evade, 52000000, 'Avoiding', ['Agent': 'Agent', 'Theme': 'Undesirable_situation', 'Location': 'Place']).
fnpattern(evade, 52000000, 'Avoiding', ['Agent': 'Agent', 'Location': 'Undesirable_situation']).
fnpattern(evade, 52000000, 'Evading', ['Agent': 'Evader', 'Theme': 'Pursuer']).
fnpattern(shun, 52000000, 'Avoiding', ['Agent': 'Agent', 'Theme': 'Undesirable_situation', 'Location': 'Place']).
fnpattern(shun, 52000000, 'Avoiding', ['Agent': 'Agent', 'Location': 'Undesirable_situation']).
fnpattern(sidestep, 52000000, 'Avoiding', ['Agent': 'Agent', 'Theme': 'Undesirable_situation', 'Location': 'Place']).
fnpattern(sidestep, 52000000, 'Avoiding', ['Agent': 'Agent', 'Location': 'Undesirable_situation']).
fnpattern(equivocate, 53010000, 'Prevarication', ['Agent': 'Speaker', 'Theme': 'Topic']).
fnpattern(delay, 53010100, 'Hindering', ['Agent': 'Agent', 'Theme': 'Action']).
fnpattern(measure, 54010000, 'Dimension', ['Theme': 'Object', 'Value': 'Measurement']).
fnpattern(total, 54010000, 'Adding_up', ['Agent': 'Cognizer', 'Theme': 'Numbers', 'Value': 'Result']).
fnpattern(total, 54010000, 'Amounting_to', ['Theme': 'Attribute', 'Value': 'Value']).
fnpattern(weigh, 54010000, 'Dimension', ['Theme': 'Object', 'Value': 'Measurement']).
fnpattern(cost, 54020000, 'Expensiveness', ['Theme': 'Goods', 'Value': 'Asset', 'Benefactor': 'Payer']).
fnpattern(contain, 54030000, 'Containing', ['Location': 'Container', 'Theme': 'Contents']).
fnpattern(hold, 54030000, 'Containing', ['Location': 'Container', 'Theme': 'Contents']).
fnpattern(assess, 54040000, 'Assessing', ['Agent': 'Assessor', 'Theme': 'Feature', 'Value': 'Value']).
fnpattern(peg, 54040000, 'Categorization', ['Agent': 'Cognizer', 'Theme': 'Item', 'Value': 'Category']).
fnpattern(rate, 54040000, 'Assessing', ['Agent': 'Assessor', 'Theme': 'Feature', 'Value': 'Value']).
fnpattern(scrimp, 54050000, 'Frugality', ['Agent': 'Resouce_controller', 'Theme': 'Resouce']).
fnpattern(begin, 55010100, 'Activity_start', ['Agent': 'Agent', 'Theme': 'Activity', 'Time': 'Time']).
fnpattern(resume, 55010100, 'Process_resume', ['Theme': 'Process', 'Time': 'Time']).
fnpattern(start, 55010100, 'Activity_start', ['Agent': 'Agent', 'Theme': 'Activity', 'Time': 'Time']).
fnpattern(start, 55010100, 'Process_start', ['Theme': 'Event', 'Time': 'Time']).
fnpattern(keep, 55010210, 'Activity_ongoing', ['Agent': 'Agent', 'Theme': 'Activity']).
fnpattern(continue, 55010221, 'Activity_ongoing', ['Agent': 'Agent', 'Theme': 'Activity']).
fnpattern(continue, 55010221, 'Process_continue', ['Theme': 'Event', 'Time': 'Time']).
fnpattern(end, 55010300, 'Process_end', ['Theme': 'Process']).
fnpattern(finish, 55010300, 'Activity_finish', ['Agent': 'Agent', 'Theme': 'Activity']).
fnpattern(discontinue, 55020000, 'Process_stop', ['Theme': 'Process']).
fnpattern(quit, 55020000, 'Process_stop', ['Theme': 'Process']).
fnpattern(halt, 55040000, 'Activity_stop', ['Agent': 'Agent', 'Theme': 'Activity', 'Instrument': 'Means']).
fnpattern(halt, 55040000, 'Halt', ['Theme': 'Activity', 'Instrument': 'Means']).
fnpattern(terminate, 55040000, 'Activity_finish', ['Agent': 'Agent', 'Theme': 'Activity', 'Instrument': 'Means']).
fnpattern(terminate, 55040000, 'Activity_stop', ['Agent': 'Agent', 'Theme': 'Activity', 'Instrument': 'Means']).
fnpattern(terminate, 55040000, 'Firing', ['Agent': 'Employer', 'Theme': 'Employee']).
fnpattern(terminate, 55040000, 'Killing', ['Agent': 'Killer', 'Theme': 'Victim', 'Instrument': 'Instrument']).
fnpattern(terminate, 55040000, 'Killing', ['Agent': 'Killer', 'Theme': 'Victim', 'Instrument': 'Means']).
fnpattern(terminate, 55040000, 'Killing', ['Agent': 'Cause', 'Theme': 'Victim', 'Instrument': 'Instrument']).
fnpattern(terminate, 55040000, 'Killing', ['Agent': 'Cause', 'Theme': 'Victim', 'Instrument': 'Means']).
fnpattern(drizzle, 57000000, 'Precipitation', ['Theme': 'Precipitation']).
fnpattern(hail, 57000000, 'Precipitation', ['Theme': 'Precipitation']).
fnpattern(rain, 57000000, 'Precipitation', ['Theme': 'Precipitation']).
fnpattern(sleet, 57000000, 'Precipitation', ['Theme': 'Precipitation']).
fnpattern(snow, 57000000, 'Precipitation', ['Theme': 'Precipitation']).
fnpattern(sprinkle, 57000000, 'Precipitation', ['Theme': 'Precipitation']).
fnpattern(storm, 57000000, 'Weather', ['': '']).
fnpattern(implore, 58000000, 'Request', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Message']).
fnpattern(implore, 58000000, 'Request', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(persuade, 58000000, 'Suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Content']).
fnpattern(persuade, 58000000, 'Suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Text']).
fnpattern(persuade, 58000000, 'Suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(urge, 58000000, 'Request', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Message']).
fnpattern(urge, 58000000, 'Request', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(commission, 59000000, 'Employing', ['Agent': 'Employer', 'Patient': 'Employee', 'Proposition': 'Field']).
fnpattern(commission, 59000000, 'Employing', ['Agent': 'Employer', 'Patient': 'Employee', 'Proposition': 'Position']).
fnpattern(commission, 59000000, 'Employing', ['Agent': 'Employer', 'Patient': 'Employee', 'Proposition': 'Task']).
fnpattern(commission, 59000000, 'Hiring', ['Agent': 'Employer', 'Patient': 'Employee', 'Proposition': 'Position']).
fnpattern(commission, 59000000, 'Hiring', ['Agent': 'Employer', 'Patient': 'Employee', 'Proposition': 'Task']).
fnpattern(dare, 59000000, 'Daring', ['Agent': 'Agent', 'Proposition': 'Action']).
fnpattern(incite, 59000000, 'Talking_into', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Content']).
fnpattern(incite, 59000000, 'Talking_into', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Goods']).
fnpattern(induce, 59000000, 'Talking_into', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Content']).
fnpattern(induce, 59000000, 'Talking_into', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Goods']).
fnpattern(mislead, 59000000, 'Prevarication', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(press, 59000000, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Content']).
fnpattern(press, 59000000, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(pressure, 59000000, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Content']).
fnpattern(pressure, 59000000, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(spur, 59000000, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Content']).
fnpattern(spur, 59000000, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(tempt, 59000000, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Content']).
fnpattern(tempt, 59000000, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(cajole, 59000100, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Content']).
fnpattern(cajole, 59000100, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(deceive, 59000100, 'Prevarication', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(fool, 59000100, 'Prevarication', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(hoodwink, 59000100, 'Prevarication', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(wheedle, 59000100, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Content']).
fnpattern(wheedle, 59000100, 'Attempt_suasion', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Topic']).
fnpattern(permit, 60000000, 'Grant_permission', ['Agent': 'Grantor', 'Patient': 'Grantee', 'Proposition': 'Action']).
fnpattern(permit, 60000000, 'Permitting', ['Agent': 'Principle', 'Patient': 'State_of_affairs']).
fnpattern(permit, 60000000, 'Permitting', ['Agent': 'Principle', 'Proposition': 'State_of_affairs']).
fnpattern(command, 60000100, 'Request', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Message']).
fnpattern(demand, 60000100, 'Request', ['Agent': 'Speaker', 'Patient': 'Addressee', 'Proposition': 'Message']).
fnpattern(aim, 62000000, 'Purpose', ['Experiencer': 'Agent', 'Theme': 'Goal']).
fnpattern(aim, 62000000, 'Purpose', ['Experiencer': 'Agent', 'Theme': 'Attribute']).
fnpattern(aim, 62000000, 'Purpose', ['Experiencer': 'Agent', 'Theme': 'Value']).
fnpattern(expect, 62000000, 'Expectation', ['Experiencer': 'Cognizer', 'Theme': 'Phenomenon']).
fnpattern(expect, 62000000, 'Expectation', ['Experiencer': 'Cognizer', 'Theme': 'Topic']).
fnpattern(intend, 62000000, 'Purpose', ['Experiencer': 'Agent', 'Theme': 'Goal']).
fnpattern(intend, 62000000, 'Purpose', ['Experiencer': 'Agent', 'Theme': 'Attribute']).
fnpattern(intend, 62000000, 'Purpose', ['Experiencer': 'Agent', 'Theme': 'Value']).
fnpattern(allow, 64000000, 'Permitting', ['Agent': 'Principle', 'Theme': 'State_of_affairs']).
fnpattern(permit, 64000000, 'Permitting', ['Agent': 'Principle', 'Theme': 'State_of_affairs']).
fnpattern(allow, 65000000, 'Permitting', ['Agent': 'Principle', 'Theme': 'State_of_affairs']).
fnpattern(allow, 65000000, 'Permitting', ['Agent': 'Principle', 'Location': 'State_of_affairs']).
fnpattern(permit, 65000000, 'Grant_permission', ['Agent': 'Grantor', 'Theme': 'Grantee', 'Location': 'Action']).
fnpattern(permit, 65000000, 'Grant_permission', ['Agent': 'Grantor', 'Theme': 'Grantee', 'Location': 'Place']).
fnpattern(permit, 65000000, 'Permitting', ['Agent': 'Principle', 'Theme': 'State_of_affairs']).
fnpattern(permit, 65000000, 'Permitting', ['Agent': 'Principle', 'Location': 'State_of_affairs']).
fnpattern(waste, 66000100, 'Frugality', ['Agent': 'Resource_controller', 'Theme': 'Resource']).
fnpattern(waste, 66000100, 'Frugality', ['Agent': 'Resource_controller', 'Theme': 'Behavior']).
fnpattern(ban, 67000000, 'Prohibiting', ['Agent': 'Principle', 'Theme': 'State_of_affairs']).
fnpattern(dissuade, 67000000, 'Suasion', ['Agent': 'Speaker', 'Theme': 'Addressee']).
fnpattern(dissuade, 67000000, 'Suasion', ['Agent': 'Speaker', 'Theme': 'Content']).
fnpattern(dissuade, 67000000, 'Suasion', ['Agent': 'Speaker', 'Theme': 'Topic']).
fnpattern(dissuade, 67000000, 'Suasion', ['Agent': 'Text', 'Theme': 'Addressee']).
fnpattern(dissuade, 67000000, 'Suasion', ['Agent': 'Text', 'Theme': 'Content']).
fnpattern(dissuade, 67000000, 'Suasion', ['Agent': 'Text', 'Theme': 'Topic']).
fnpattern(forbid, 67000000, 'Deny_permission', ['Agent': 'Authority', 'Theme': 'Protagonist']).
fnpattern(forbid, 67000000, 'Deny_permission', ['Agent': 'Authority', 'Theme': 'Action']).
fnpattern(forbid, 67000000, 'Prohibiting', ['Agent': 'Principle', 'Theme': 'State_of_affairs']).
fnpattern(prevent, 67000000, 'Preventing', ['Agent': 'Preventing_cause', 'Theme': 'Event']).
fnpattern(prevent, 67000000, 'Thwarting', ['Agent': 'Preventing_cause', 'Theme': 'Protagonist']).
fnpattern(prevent, 67000000, 'Thwarting', ['Agent': 'Preventing_cause', 'Theme': 'Action']).
fnpattern(prohibit, 67000000, 'Deny_permission', ['Agent': 'Authority', 'Theme': 'Protagonist']).
fnpattern(prohibit, 67000000, 'Deny_permission', ['Agent': 'Authority', 'Theme': 'Action']).
fnpattern(prohibit, 67000000, 'Preventing', ['Agent': 'Preventing_cause', 'Theme': 'Event']).
fnpattern(prohibit, 67000000, 'Prohibiting', ['Agent': 'Principle', 'Theme': 'State_of_affairs']).
fnpattern(waste, 68000000, 'Frugality', ['Agent': 'Resource_controller', 'Asset': 'Resource', 'Theme': 'Behavior']).
fnpattern(desist, 69000000, 'Avoiding', ['Agent': 'Agent', 'Theme': 'Undesirable_situation']).
fnpattern(desist, 69000000, 'Process_stop', ['Agent': 'Cause', 'Theme': 'Process']).
fnpattern(desist, 69000000, 'Process_stop', ['Agent': 'Reason', 'Theme': 'Process']).
fnpattern(depend, 70000000, 'Reliance', ['Agent': 'Protagonist', 'Theme': 'Means']).
fnpattern(depend, 70000000, 'Reliance', ['Agent': 'Protagonist', 'Theme': 'Instrument']).
fnpattern(depend, 70000000, 'Reliance', ['Agent': 'Protagonist', 'Theme': 'Intermediary']).
fnpattern(depend, 70000000, 'Reliance', ['Agent': 'Protagonist', 'Theme': 'Benefit']).
fnpattern(depend, 70000000, 'Reliance', ['Agent': 'Protagonist', 'Theme': 'Purpose']).
fnpattern(rely, 70000000, 'Reliance', ['Agent': 'Protagonist', 'Theme': 'Means']).
fnpattern(rely, 70000000, 'Reliance', ['Agent': 'Protagonist', 'Theme': 'Instrument']).
fnpattern(rely, 70000000, 'Reliance', ['Agent': 'Protagonist', 'Theme': 'Intermediary']).
fnpattern(rely, 70000000, 'Reliance', ['Agent': 'Protagonist', 'Theme': 'Benefit']).
fnpattern(rely, 70000000, 'Reliance', ['Agent': 'Protagonist', 'Theme': 'Purpose']).
fnpattern(collude, 71000000, 'Collaboration', ['Actor': 'Partners', 'Actor1': 'Partner_1', 'Actor2': 'Partner_2', 'Theme': 'Undertaking']).
fnpattern(conspire, 71000000, 'Collaboration', ['Actor': 'Partners', 'Actor1': 'Partner_1', 'Actor2': 'Partner_2', 'Theme': 'Undertaking']).
fnpattern(retaliate, 71000000, 'Revenge', ['Actor': 'Avenger', 'Actor2': 'Offender', 'Theme': 'Punishment']).
fnpattern(retaliate, 71000000, 'Revenge', ['Actor': 'Avenger', 'Actor2': 'Injury', 'Theme': 'Punishment']).
fnpattern(retaliate, 71000000, 'Revenge', ['Actor': 'Avenger', 'Actor2': 'Injured_Party', 'Theme': 'Punishment']).
fnpattern(retaliate, 71000000, 'Revenge', ['Actor': 'Avenger', 'Actor2': 'Instrument', 'Theme': 'Punishment']).
fnpattern(retaliate, 71000000, 'Revenge', ['Actor1': 'Avenger', 'Actor2': 'Offender', 'Theme': 'Punishment']).
fnpattern(retaliate, 71000000, 'Revenge', ['Actor1': 'Avenger', 'Actor2': 'Injury', 'Theme': 'Punishment']).
fnpattern(retaliate, 71000000, 'Revenge', ['Actor1': 'Avenger', 'Actor2': 'Injured_Party', 'Theme': 'Punishment']).
fnpattern(retaliate, 71000000, 'Revenge', ['Actor1': 'Avenger', 'Actor2': 'Instrument', 'Theme': 'Punishment']).
fnpattern(sin, 71000000, 'Misdeed', ['Actor': 'Wrongdoer', 'Actor2': 'Injured_party', 'Theme': 'Misdeed']).
fnpattern(sin, 71000000, 'Misdeed', ['Actor1': 'Wrongdoer', 'Actor2': 'Injured_party', 'Theme': 'Misdeed']).
fnpattern(team_up, 71000000, 'Collaboration', ['Actor': 'Partners', 'Actor1': 'Partner_1', 'Actor2': 'Partner_2', 'Theme': 'Undertaking']).
fnpattern(succor, 72000000, 'Assistance', ['Agent': 'Helper', 'Beneficiary': 'Benefitted_party', 'Theme': 'Goal']).
fnpattern(succor, 72000000, 'Assistance', ['Agent': 'Helper', 'Beneficiary': 'Benefitted_party', 'Theme': 'Focal_entity']).
fnpattern(abet, 72000100, 'Assistance', ['Agent': 'Helper', 'Beneficiary': 'Benefitted_party', 'Theme': 'Goal']).
fnpattern(abet, 72000100, 'Assistance', ['Agent': 'Helper', 'Beneficiary': 'Benefitted_party', 'Theme': 'Focal_entity']).
fnpattern(assist, 72000100, 'Assistance', ['Agent': 'Helper', 'Beneficiary': 'Benefitted_party', 'Theme': 'Goal']).
fnpattern(assist, 72000100, 'Assistance', ['Agent': 'Helper', 'Beneficiary': 'Benefitted_party', 'Theme': 'Focal_entity']).
fnpattern(help, 72000100, 'Assistance', ['Agent': 'Helper', 'Beneficiary': 'Benefitted_party', 'Theme': 'Goal']).
fnpattern(help, 72000100, 'Assistance', ['Agent': 'Helper', 'Beneficiary': 'Benefitted_party', 'Theme': 'Focal_entity']).
fnpattern(participate, 73000200, 'Participation', ['Actor': 'Participants', 'Actor1': 'Participant_1', 'Actor2': 'Participant_2', 'Theme': 'Event']).
fnpattern(participate, 73000200, 'Participation', ['Actor': 'Participants', 'Actor1': 'Participant_1', 'Actor2': 'Participant_2', 'Theme': 'Institution']).
fnpattern(confine, 76000000, 'Cause_confinement', ['Cause': 'Agent', 'Patient': 'Theme', 'Proposition': 'Goal']).
fnpattern(confine, 76000000, 'Cause_confinement', ['Cause': 'Cause', 'Patient': 'Theme', 'Proposition': 'Goal']).
fnpattern(constrain, 76000000, 'Hindering', ['Cause': 'Agent', 'Patient': 'Item', 'Proposition': 'Attribute']).
fnpattern(constrain, 76000000, 'Hindering', ['Cause': 'Agent', 'Patient': 'Item', 'Proposition': 'Path']).
fnpattern(constrain, 76000000, 'Hindering', ['Cause': 'Agent', 'Patient': 'Item', 'Proposition': 'Value_1']).
fnpattern(constrain, 76000000, 'Hindering', ['Cause': 'Agent', 'Patient': 'Item', 'Proposition': 'Value_2']).
fnpattern(constrain, 76000000, 'Hindering', ['Cause': 'Agent', 'Patient': 'Item', 'Proposition': 'Means']).
fnpattern(constrain, 76000000, 'Hindering', ['Cause': 'Cause', 'Patient': 'Item', 'Proposition': 'Attribute']).
fnpattern(constrain, 76000000, 'Hindering', ['Cause': 'Cause', 'Patient': 'Item', 'Proposition': 'Path']).
fnpattern(constrain, 76000000, 'Hindering', ['Cause': 'Cause', 'Patient': 'Item', 'Proposition': 'Value_1']).
fnpattern(constrain, 76000000, 'Hindering', ['Cause': 'Cause', 'Patient': 'Item', 'Proposition': 'Value_2']).
fnpattern(constrain, 76000000, 'Hindering', ['Cause': 'Cause', 'Patient': 'Item', 'Proposition': 'Means']).
fnpattern(restrict, 76000000, 'Cause_confinement', ['Cause': 'Agent', 'Patient': 'Theme', 'Proposition': 'Goal']).
fnpattern(restrict, 76000000, 'Cause_confinement', ['Cause': 'Cause', 'Patient': 'Theme', 'Proposition': 'Goal']).
fnpattern(understand, 77000000, 'Awareness', ['Agent': 'Cognizer', 'Proposition': 'Content']).
fnpattern(understand, 77000000, 'Awareness', ['Agent': 'Cognizer', 'Proposition': 'Topic']).
fnpattern(imply, 78000000, 'Evidence', ['Cause': 'Support', 'Recipient': 'Cognizer', 'Topic': 'Proposition']).
fnpattern(predict, 78000000, 'Predicting', ['Cause': 'Evidence', 'Topic': 'Eventuality']).
fnpattern(predict, 78000000, 'Predicting', ['Cause': 'Speaker', 'Topic': 'Eventuality']).
fnpattern(indicate, 78000100, 'Communication', ['Cause': 'Communicator', 'Recipient': 'Addressee', 'Topic': 'Message']).
fnpattern(indicate, 78000100, 'Communication', ['Cause': 'Communicator', 'Recipient': 'Addressee', 'Topic': 'Topic']).
fnpattern(indicate, 78000100, 'Evidence', ['Cause': 'Support', 'Recipient': 'Cognizer', 'Topic': 'Proposition']).
fnpattern(indicate, 78000100, 'Sign', ['Cause': 'Indicator', 'Topic': 'Indicated']).
fnpattern(confirm, 78000110, 'Evidence', ['Cause': 'Support', 'Recipient': 'Cognizer', 'Topic': 'Proposition']).
fnpattern(confirm, 78000110, 'Statement', ['Cause': 'Speaker', 'Recipient': 'Addressee', 'Topic': 'Message']).
fnpattern(confirm, 78000110, 'Statement', ['Cause': 'Speaker', 'Recipient': 'Addressee', 'Topic': 'Topic']).
fnpattern(confirm, 78000110, 'Verification', ['Cause': 'Inspector', 'Topic': 'Unconfirmed_content']).
fnpattern(confirm, 78000110, 'Verification', ['Cause': 'Medium', 'Topic': 'Unconfirmed_content']).
fnpattern(prove, 78000110, 'Evidence', ['Cause': 'Support', 'Recipient': 'Cognizer', 'Topic': 'Proposition']).
fnpattern(prove, 78000110, 'Reasoning', ['Cause': 'Support', 'Recipient': 'Addressee', 'Topic': 'Content']).
fnpattern(prove, 78000110, 'Reasoning', ['Cause': 'Arguer', 'Recipient': 'Addressee', 'Topic': 'Content']).
fnpattern(acquit, 80000100, 'Verdict', ['Cause': 'Judge', 'Source': 'Defendant', 'Theme': 'Charges']).
fnpattern(acquit, 80000100, 'Verdict', ['Cause': 'Reason', 'Source': 'Defendant', 'Theme': 'Charges']).
fnpattern(accuse, 81000000, 'Judgement', ['Agent': 'Cognizer', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(accuse, 81000000, 'Judgement_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Reason']).
fnpattern(accuse, 81000000, 'Judgement_communication', ['Agent': 'Communicator', 'Theme': 'Evaluee', 'Predicate': 'Topic']).
fnpattern(accuse, 81000000, 'Notification_of_charges', ['Agent': 'Arraign_authority', 'Theme': 'Accused', 'Predicate': 'Charges']).
fnpattern(retire, 82000300, 'Quitting', ['Agent': 'Employee', 'Source': 'Position']).
fnpattern(retire, 82000300, 'Quitting', ['Agent': 'Employee', 'Source': 'Field']).
fnpattern(retreat, 82000300, 'Departing', ['Agent': 'Theme', 'Source': 'Source']).
fnpattern(retreat, 82000300, 'Quitting_a_place', ['Agent': 'Self_mover', 'Source': 'Source']).
fnpattern(learn, 84000110, 'Becoming_aware', ['Agent': 'Cognizer', 'Theme': 'Phenomenon', 'Source': 'Means']).
fnpattern(learn, 84000110, 'Becoming_aware', ['Agent': 'Cognizer', 'Theme': 'Phenomenon', 'Source': 'Instrument']).
fnpattern(learn, 84000110, 'Becoming_aware', ['Agent': 'Cognizer', 'Theme': 'Topic', 'Source': 'Means']).
fnpattern(learn, 84000110, 'Becoming_aware', ['Agent': 'Cognizer', 'Theme': 'Topic', 'Source': 'Instrument']).
fnpattern(learn, 84000110, 'Coming_to_believe', ['Agent': 'Cognizer', 'Theme': 'Content', 'Source': 'Evidence']).
fnpattern(learn, 84000110, 'Coming_to_believe', ['Agent': 'Cognizer', 'Theme': 'Content', 'Source': 'Means']).
fnpattern(learn, 84000110, 'Coming_to_believe', ['Agent': 'Cognizer', 'Theme': 'Topic', 'Source': 'Evidence']).
fnpattern(learn, 84000110, 'Coming_to_believe', ['Agent': 'Cognizer', 'Theme': 'Topic', 'Source': 'Means']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Subject', 'Source': 'Teacher']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Subject', 'Source': 'Institution']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Subject', 'Source': 'Course']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Subject', 'Source': 'Material']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Skill', 'Source': 'Teacher']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Skill', 'Source': 'Institution']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Skill', 'Source': 'Course']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Skill', 'Source': 'Material']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Role', 'Source': 'Teacher']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Role', 'Source': 'Institution']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Role', 'Source': 'Course']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Role', 'Source': 'Material']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Precept', 'Source': 'Teacher']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Precept', 'Source': 'Institution']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Precept', 'Source': 'Course']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Precept', 'Source': 'Material']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Fact', 'Source': 'Teacher']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Fact', 'Source': 'Institution']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Fact', 'Source': 'Course']).
fnpattern(learn, 84000110, 'Education_teaching', ['Agent': 'Student', 'Theme': 'Fact', 'Source': 'Material']).
fnpattern(learn, 84000110, 'Memorization', ['Agent': 'Cognizer', 'Theme': 'Pattern']).
fnpattern(defend, 85000000, 'Defend', ['Agent': 'Defender', 'Theme': 'Victim', 'Proposition': 'Assailant']).
fnpattern(defend, 85000000, 'Defend', ['Agent': 'Defender', 'Proposition': 'Victim']).
fnpattern(defend, 85000000, 'Justifying', ['Agent': 'Agent', 'Theme': 'Act', 'Proposition': 'State_of_affairs']).
fnpattern(defend, 85000000, 'Justifying', ['Agent': 'Agent', 'Proposition': 'Act']).
fnpattern(wonder, 88000100, 'Cogitation', ['Eperiencer': 'Cognizer', 'Cause': 'Topic']).
fnpattern(cohere, 89000000, 'Compatibility', ['Actor': 'Item_1', 'Proposition': 'Item_2']).
fnpattern(cohere, 89000000, 'Compatibility', ['Actor': 'Item_1', 'Proposition': 'Parameter']).
fnpattern(cohere, 89000000, 'Compatibility', ['Actor': 'Items', 'Proposition': 'Item_2']).
fnpattern(cohere, 89000000, 'Compatibility', ['Actor': 'Items', 'Proposition': 'Parameter']).
fnpattern(jibe, 89000000, 'Compatibility', ['Actor': 'Item_1', 'Proposition': 'Item_2']).
fnpattern(jibe, 89000000, 'Compatibility', ['Actor': 'Item_1', 'Proposition': 'Parameter']).
fnpattern(jibe, 89000000, 'Compatibility', ['Actor': 'Items', 'Proposition': 'Item_2']).
fnpattern(jibe, 89000000, 'Compatibility', ['Actor': 'Items', 'Proposition': 'Parameter']).
fnpattern(square, 89000000, 'Compatibility', ['Actor': 'Item_1', 'Proposition': 'Item_2']).
fnpattern(square, 89000000, 'Compatibility', ['Actor': 'Item_1', 'Proposition': 'Parameter']).
fnpattern(square, 89000000, 'Compatibility', ['Actor': 'Items', 'Proposition': 'Item_2']).
fnpattern(square, 89000000, 'Compatibility', ['Actor': 'Items', 'Proposition': 'Parameter']).
fnpattern(outstrip, 90000000, 'Surpassing', ['Theme1': 'Profiled_item', 'Theme2': 'Standard_item', 'Attribute': 'Attribute']).
fnpattern(outstrip, 90000000, 'Surpassing', ['Theme1': 'Profiled_item', 'Theme2': 'Standard_item', 'Attribute': 'Extent']).
fnpattern(outstrip, 90000000, 'Surpassing', ['Theme1': 'Profiled_item', 'Theme2': 'Standard_item', 'Attribute': 'Standard_attribute']).
fnpattern(outstrip, 90000000, 'Surpassing', ['Theme1': 'Profiled_item', 'Theme2': 'Standard_item', 'Attribute': 'Profiled_attribute']).
fnpattern(surpass, 90000000, 'Surpassing', ['Theme1': 'Profiled_item', 'Theme2': 'Standard_item', 'Attribute': 'Attribute']).
fnpattern(surpass, 90000000, 'Surpassing', ['Theme1': 'Profiled_item', 'Theme2': 'Standard_item', 'Attribute': 'Extent']).
fnpattern(surpass, 90000000, 'Surpassing', ['Theme1': 'Profiled_item', 'Theme2': 'Standard_item', 'Attribute': 'Standard_attribute']).
fnpattern(surpass, 90000000, 'Surpassing', ['Theme1': 'Profiled_item', 'Theme2': 'Standard_item', 'Attribute': 'Profiled_attribute']).
fnpattern(commit, 92000100, 'Institutionalization', ['Agent': 'Authority', 'Theme': 'Patient', 'Destination': 'Facility']).
fnpattern(confine, 92000100, 'Cause_confinement', ['Agent': 'Agent', 'Theme': 'Theme', 'Destination': 'Goal']).
fnpattern(confine, 92000100, 'Cause_confinement', ['Agent': 'Cause', 'Theme': 'Theme', 'Destination': 'Goal']).
fnpattern(venture, 94000000, 'Daring', ['Agent': 'Agent', 'Theme': 'Action']).
fnpattern(venture, 94000000, 'Statement', ['Agent': 'Speaker', 'Theme': 'Message']).
fnpattern(venture, 94000000, 'Statement', ['Agent': 'Speaker', 'Theme': 'Medium']).
fnpattern(venture, 94000000, 'Statement', ['Agent': 'Speaker', 'Theme': 'Topic']).
fnpattern(chance, 94000100, 'Daring', ['Agent': 'Agent', 'Theme': 'Action']).
fnpattern(hazard, 94000100, 'Daring', ['Agent': 'Agent', 'Theme': 'Action']).
fnpattern(hazard, 94000100, 'Statement', ['Agent': 'Speaker', 'Theme': 'Message']).
fnpattern(hazard, 94000100, 'Statement', ['Agent': 'Speaker', 'Theme': 'Medium']).
fnpattern(hazard, 94000100, 'Statement', ['Agent': 'Speaker', 'Theme': 'Topic']).
fnpattern(risk, 94000100, 'Daring', ['Agent': 'Agent', 'Theme': 'Action']).
