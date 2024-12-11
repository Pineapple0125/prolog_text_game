/* Adventure Game by Bonan Mai */

:- dynamic 
        i_am_at/1, 
        at/2, 
        holding/1, 
        alive/0, 
        enemy_alive/2,
        is_enemy/1,
        army_alive/0,
        package/1, 
        friend/1,
        player_health/1, 
        player_attack/1, 
        enemy_probability/3, 
        enemy_attack/2,
        enemy_health/2,
        weapon_attack/2.  

:- discontiguous interact/1, describe/1.

:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)), retractall(alive), retractall(player_attack(_)), retractall(player_health(_)), retractall(package(_)), retractall(friend(_)).

:- use_module(library(pce)).
    
n :- go(north).
s :- go(south).
e :- go(east).
w :- go(west).

i_am_at(born_room).
alive.
holding(hand).


path(born_room, south, freshman_room).
path(freshman_room, north, born_room).
path(freshman_room, south, central_room).
path(central_room, north, freshman_room).
path(central_room, west, shop_room).
path(shop_room, east, central_room).
path(central_room, east, small_boss_room).
path(small_boss_room, west, central_room).
path(central_room, south, final_boss_room).
path(final_boss_room, north, central_room).
path(final_boss_room, south, treasure_room).
path(treasure_room, north, final_boss_room).

enemy_alive(zombie, alive).
enemy_alive(soul_eater, alive).
enemy_alive(queen_of_the_moonlit, alive).
enemy_alive(army, alive).

at(zombie, freshman_room).
at(branch, freshman_room).
at(the_map, central_room).
at(army, shop_room).
at(businessman, shop_room).
at(soul_eater, small_boss_room).
at(queen_of_the_moonlit, final_boss_room).
at(treasure, treasure_room).




/* initialize the dynamic npc attribute*/

initialize_attribute :-

        assert(player_health(2)),
        assert(player_attack(1)),
        assert(friend_attack(0)),


        /* Randomly select three different elements for the zombie */


        random_select(Element1, [grass, fire, water], Remaining1),
        random_select(Element2, Remaining1, Remaining2),
        random_select(Element3, Remaining2, _),
        assert(enemy_element_first(zombie, Element1)),
        assert(enemy_element_second(zombie, Element2)),
        assert(enemy_element_third(zombie, Element3)),
        assert(enemy_attack(zombie, 1)),
        assert(enemy_health(zombie, 2)),
        assert(enemy_probability(zombie, Element1, 90)),  % first element 60%
        assert(enemy_probability(zombie, Element2, 5)),  % second element 30%
        assert(enemy_probability(zombie, Element3, 5)),  % third element 10%

        /* Randomly select three different elements for the small_boss */
        random_select(Element4, [grass, fire, water], Remaining3),
        random_select(Element5, Remaining3, Remaining4),
        random_select(Element6, Remaining4, _),
        assert(enemy_element_first(soul_eater, Element4)),
        assert(enemy_element_second(soul_eater, Element5)),
        assert(enemy_element_third(soul_eater, Element6)),
        assert(enemy_attack(soul_eater, 5)),
        assert(enemy_health(soul_eater, 8)),
        assert(enemy_probability(soul_eater, Element4, 60)), 
        assert(enemy_probability(soul_eater, Element5, 30)), 
        assert(enemy_probability(soul_eater, Element6, 10)),  

        /* Randomly select three different elements for the final_boss */
        random_select(Element7, [grass, fire, water], Remaining5),
        random_select(Element8, Remaining5, Remaining6),
        random_select(Element9, Remaining6, _),
        assert(enemy_element_first(queen_of_the_moonlit, Element7)),
        assert(enemy_element_second(queen_of_the_moonlit, Element8)),
        assert(enemy_element_third(queen_of_the_moonlit, Element9)),
        assert(enemy_attack(queen_of_the_moonlit, 8)),
        assert(enemy_health(queen_of_the_moonlit, 12)),
        assert(enemy_probability(queen_of_the_moonlit, Element7, 40)),  
        assert(enemy_probability(queen_of_the_moonlit, Element8, 40)),  
        assert(enemy_probability(queen_of_the_moonlit, Element9, 20)),  

        /* Randomly select three different elements for the army */
        random_select(Element10, [grass, fire, water], Remaining7),
        random_select(Element11, Remaining7, Remaining8),
        random_select(Element12, Remaining8, _),
        assert(enemy_element_first(army, Element10)),
        assert(enemy_element_second(army, Element11)),
        assert(enemy_element_third(army, Element12)),
        assert(enemy_attack(army, 3)),
        assert(enemy_health(army, 2)),
        assert(enemy_probability(army, Element10, 75)),  
        assert(enemy_probability(army, Element11, 15)),  
        assert(enemy_probability(army, Element12, 10)),  

        assert(is_enemy(army)),
        assert(is_enemy(zombie)),
        assert(is_enemy(soul_eater)),
        assert(is_enemy(queen_of_the_moonlit)),
        assert(is_enemy(businessman)),

        assert(weapon_attack(hand, 0)),
        assert(weapon_attack(branch, 1)),
        assert(weapon_attack(dagger, 2)),
        assert(weapon_attack(greatsword, 4)),
        assert(weapon_attack(soulsword, 9)).




take(Item) :-
        package(Item),
        write("You are already having it!"),
        !, nl.

take(Item) :-
        i_am_at(Here),
        at(Item, Here) ->      
        assert(package(Item)),  
        
        write('You get the '), 
        write(Item), 
        write('.'), nl,
        check(Item),  !
    ;
        write('You cannot take the '), 
        write(Item), 
        write(' because it is not here.'), nl, !.

take(_) :-
        write('I don''t see it here.'), nl.

hold(Weapon) :-
        holding(Weapon),
        write('You are already holding it!'),
        !, nl.

hold(Weapon) :-
        \+ package(Weapon),
        write("You need to pick up the "), write(Weapon), write(" first!"), nl, !.

hold(Weapon) :-
         package(Weapon),
    ( 
        % Check if player is already holding a weapon
        holding(CurrentWeapon) ->
            retract(holding(CurrentWeapon)),
            write("Replacing weapon: "), write(CurrentWeapon), write(" with "), write(Weapon), nl
        ;
            write("Equipping weapon: "), write(Weapon), nl
    ),

        assert(holding(Weapon)),

        weapon_attack(Weapon, Weapon_value),
        friend_attack(Friend_value),
        player_attack(PlayerAttack),
        NewPlayerAttack is PlayerAttack + Friend_value + Weapon_value,

        retract(player_attack(PlayerAttack)),
        assert(player_attack(NewPlayerAttack)),
        
        write("Now holding the weapon: "), write(Weapon),
        !, nl.



check(Item) :-
        package(Item) ->
        describe(Item)
        ;
        write('You do not have the '), write(Item), write(' in your backpack.'), nl.



drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('You dropped the '), write(X), write('.'), nl,
        !, nl.

drop(_) :-
        write('You aren''t holding it!'), nl.



view_package :-
    package(Items),  
    (   Items \= []
    ->  write('The items in the backpack are:'), nl,
        print_items(Items)  
    ;   write('The backpack is empty.'), nl
    ).


items_in_room(Room) :-
    findall(Item, at(Item, Room), Items),
    (Items \= [] -> 
        write('Items in '), write(Room), write(': '), writeln(Items);
        write('No items in '), write(Room), writeln('.')
    ).


enemy_dead(queen_of_the_moonlit) :-
    i_am_at(Here),
    assert(at(lunar_tear, Here)),
    assert(at(the_supreme_key, Here)),
    retract(enemy_alive(queen_of_the_moonlit, _)),
    assert(enemy_alive(queen_of_the_moonlit, dead)),
    format('~w dropped: ~w and ~w, items are placed in ~w.~n', [queen_of_the_moonlit, lunar_tear, the_supreme_key, Here]),
    
    final_boss_dead.

enemy_dead(Enemy) :-
    i_am_at(Here),
    (Enemy = zombie -> Item1 = bloodied_letter, Item2 = dagger
    ; Enemy = soul_eater -> Item1 = queen_of_the_moonlit_strategy, Item2 = soulsword
    ; Enemy = army -> Item1 = gold_coin, Item2 = diary
    ; Item1 = unknown_item, Item2 = unknown_item),
    assert(at(Item1, Here)),
    assert(at(Item2, Here)),

    retract(enemy_alive(Enemy, alive)),
    assert(enemy_alive(Enemy, dead)),
    format('~w dropped: ~w and ~w, items are placed in front of you', [Enemy, Item1, Item2]).

final_boss_dead :-
    enemy_alive(army, State),
    (   State = alive ->
        (   friend(army) ->
            write("The army  approaches you and offers to trade gold coins in exchange for the 'Lunar Tear'"), nl,
            write("You have three options: "), nl,
            write("1. Type '1.' to accept the trade."), nl,
            write("2. Type '2.' to kill the soldier."), nl,
            write("3. Type '3.' to refuse the offer."), nl,
            read(Choice),
            handle_friend_luner_choice(Choice)
            ;

            write("You see a soldier running from a distance"), nl,
            write("He wants to buy your Lunar Tear for one gold coin"), nl,
            write("1. Type '1.' to accept the trade."), nl,
            write("2. Type '2.' Or he'll probably kill you"), nl,
            read(Choice),
            handle_luner_choice(Choice)
        )
    ;   State = dead ->
        write('The final boss has been defeated! You feel a surge of energy as the final treasure is revealed.'), nl
    ).


handle_friend_luner_choice(1) :-
        write("You have traded the Lunar Tear for a hefty sum of gold coins."), nl,
        write("The army took the lunar tear and left, running to the north"), nl,
        assert(package(gold_coin)),
        retract(friend(army)),
        holding(Weapon),
        weapon_attack(Weapon, Weapon_value),
        retract(friend_attack(_)),
        assert(friend_attack(0)),
        NewPlayerAttact is 1 + Weapon_value,
        retract(player_attack(_PlayerAttack)),
        assert(player_attack(NewPlayerAttact)),
        retract(at(army,_)),
        retract(at(lunar_tear,_)),
        assert(at(army, freshman_room)),
        assert(at(lunar_tear, freshman_room)).

handle_friend_luner_choice(2) :-
        write("You chose to try and kill him"),
        holding(Weapon),
        weapon_attack(Weapon, Weapon_value),
        retract(friend_attack(_)),
        assert(friend_attack(0)),
        NewPlayerAttact is 1 + Weapon_value,
        retract(friend(army)),
        retract(player_attack(_PlayerAttack)),
        assert(player_attack(NewPlayerAttact)), !,
        fight(army).


handle_friend_luner_choice(3) :-
        write("army looks at you with a sad face, like he's going to say something but doesn't say anything"), nl.



handle_luner_choice(1) :-
        write("You have traded the Lunar Tear for a hefty sum of gold coins."), nl,
        write("The army took the lunar tear and left, running to the north"), nl,
        assert(package(gold_coin)),
        retract(at(army,_)),
        retract(at(lunar_tear,_)),
        assert(at(army, freshman_room)),
        assert(at(lunar_tear, freshman_room)).

handle_luner_choice(2) :-
        write("You chose to try and kill him"),
        fight(army).






go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look, nl.


go(_) :-
        write('You can''t go that way.'), nl.

look :-
        i_am_at(Place),
        describe(Place), nl, !.

/*If I have a map*/


player_die :-
        retract(alive),
        write('You have died. Game over.'), nl,
        finish.


finish :-
        nl,
        write('The game is over. Please enter the "halt." command to quit.'), nl.



display_package :-
    findall(Item, package(Item), Items),
    (   Items = []
    ->  write('your package is empty'), nl
    ;   write('You have the following items in your package:'), nl,
        forall(member(Item, Items), (write('['), write(Item), write(']'), nl))
    ), 
    !.




choose_enemy_element(Enemy, SelectElement) :-


        enemy_element_first(Enemy, E1),
        enemy_element_second(Enemy, E2),
        enemy_element_third(Enemy, E3),

        enemy_probability(Enemy, E1, Prob1),
        enemy_probability(Enemy, E2, Prob2),
        
        random(1, 101, Roll), !,  
        (Roll =< Prob1 -> SelectElement = E1 ; Roll =< Prob1+Prob2 -> SelectElement = E2 ;  SelectElement = E3).



use_godEye(PlayerAttack, EnemyAttack, ActualElement) :-

        AttackDifference is PlayerAttack - EnemyAttack,
        PredictionChance is max(min(100, 50 + AttackDifference * 10), 0),
        /* Base 50%, each point of attack difference increases by 10% */
        
        random(1, 101, Roll), !,


        (   PredictionChance = 0 ->
                write('unable to predict'),
                PredictedElement = unknown
                
        ;   Roll =< PredictionChance ->
                PredictedElement = ActualElement
                
        ;   
                Elements = [grass, fire, water],
                select(ActualElement, Elements, RemainingElements),
                random_member(PredictedElement, RemainingElements)
                
        ),

        format('God Eye prediction success rate: ~w%~n', [PredictionChance]),
        format('God Eye predicts the enemy will use the element: ~w~n', [PredictedElement]).



fight(Enemy) :-
    i_am_at(Here),
    at(Enemy, EnemyPosition),

    
    (   Here \= EnemyPosition ->
        write('You are not in the same location as the enemy. You cannot fight.'), nl,
        !
    ;   
        player_health(PlayerHealth),
        player_attack(PlayerAttack),
        enemy_attack(Enemy, EnemyAttack),
        enemy_health(Enemy, EnemyHealth),
        choose_enemy_element(Enemy, ActualEnemyElement),

        (   PlayerHealth =< 0 ->
            write('Player failed, game over.'), nl,
            player_die, !
        ;   EnemyHealth =< 0 ->
            write('Player wins, the enemy is defeated.'), nl,
            enemy_dead(Enemy)
        ;   
            write('Current status:'), nl,
            write('Player health: '), write(PlayerHealth), nl,
            write('Player attact: '), write(PlayerAttack), nl,
            write('Enemy health: '), write(EnemyHealth), nl,
            write('Enemy attact: '), write(EnemyAttack), nl,

            use_godEye(PlayerAttack, EnemyAttack, ActualEnemyElement),

            write('Please choose your element for this turn (grass, fire, water) to attack:'), nl,
            read(PlayerElement),

            (   win_condition(PlayerElement, ActualEnemyElement) ->
                write('You have won this battle!'), nl,
                ( EnemyAttack >= PlayerAttack ->
                    format('You took 1 damage~n')
                ; PlayerAttack > EnemyAttack ->
                    Damage is PlayerAttack - EnemyAttack,
                    format('You took ~w damage~n', [Damage])
                ; format('No damage taken~n')
                ),

                NewPlayerHealth is PlayerHealth,
                NewEnemyHealth is max(EnemyHealth + min(EnemyAttack - PlayerAttack, -1), 0),
                retract(player_health(PlayerHealth)),
                retract(enemy_health(Enemy, EnemyHealth)),
                assertz(player_health(NewPlayerHealth)),
                assertz(enemy_health(Enemy, NewEnemyHealth)),
                fight(Enemy)
            ;   PlayerElement = ActualEnemyElement ->
                write('This battle is a draw.'), nl,
                fight(Enemy)
            ; 
                write('You have lost the advantage this turn.'), nl,

                NewPlayerHealth is max(PlayerHealth + min(PlayerAttack - EnemyAttack, -1), 0),
                NewEnemyHealth is EnemyHealth,
                retract(player_health(PlayerHealth)),
                retract(enemy_health(Enemy, EnemyHealth)),
                assertz(player_health(NewPlayerHealth)),
                assertz(enemy_health(Enemy, NewEnemyHealth)),
                fight(Enemy)
            )
        )
    ).
        

win_condition(fire, grass).
win_condition(grass, water).
win_condition(water, fire).




interact(army) :-
        i_am_at(shop_room),
        write('You see a army. What will you do?'), nl,
        write('1. Fight the army.'), nl,
        write('2. Befriend the army.'), nl,
        write("3. Leave."), nl,
        read(Choice),
        handle_army_choice(Choice).

handle_army_choice(1) :-
        write('You chose to fight the army!'), nl,
        fight(army).

handle_army_choice(2) :-
        write('You chose to befriend the army!'), nl,
        holding(Weapon),
        weapon_attack(Weapon, Weapon_value),
        player_attack(PlayerAttack),
        retract(friend_attack(_)),
        assert(friend_attack(3)),
        friend_attack(Firend_value),
        NewPlayerAttact is PlayerAttack + Firend_value + Weapon_value,
        assert(friend(army)),
        retract(player_attack(_PlayerAttack)),
        assert(player_attack(NewPlayerAttact)).

handle_army_choice(3) :-
        write('You decide to leave.'), nl, !.



interact(businessman) :-
        write('You see a businessman. What will you do?'), nl,
        write('1. Buy strategy (cost 1 gold coins).'), nl,
        write('2. Leave.'), nl,
        read(Choice),
        handle_businessman_choice(Choice).


handle_businessman_choice(1) :- 
        package(gold_coin) ->
                (retract(package(gold_coin)),
                assert(gold_coin(new_gold_coins)),
                assert(package(soul_eater_strategy)), 
                write('You bought the soul_eater_strategy!'), nl)
        ;
                write('You don''t have enough gold coins!'), nl, !.

handle_businessman_choice(2) :-
        write('You decide to leave without buying anything.'), nl, !.






open(treasure_box) :-
        package(the_supreme_key) -> (
        write('You has the key. What will you do?'), nl,
        write('1. Open the treasure box!!!.'), nl,
        write('2. Leave.'), nl,
        read(Choice),   
        handle_treasure_choice(Choice)
        );
        write("You need the special key to open the chest."), !.


handle_treasure_choice(1) :-
    enemy_alive(army, State),
    (   State = alive,
        (   friend(army) ->
            write("As you're captivated by the dazzling jewels in the treasure chest, you don't notice the army behind you raising a long sword and thrusting it into your back. In shock, you turn to face your friend."), nl,
            write("Army says to you, 'I'm sorry, I must obtain the Lunar Tear to save the one I love. I can't let you take it away.'"), nl,
            write("But little does he know, the one he loves has already transformed completely into a zombie…and was killed by you."), nl,
            player_die, !
        )
    ;   State = dead ->
        write("You've got a beautiful collection of jewels."), nl,
        write("You can type 'finish' to finish the game, or you can walk around."), nl
    ).


handle_treasure_choice(2) :-
        write('You decide to leave without buying anything.'), nl, !.



describe(branch) :-
        write("It's an ordinary branch, but it looks like it could be used as a weapon,  type hold(Weapon). to hold it!"), !.

describe(dagger) :-
        write("A dagger with a little blood on it, but still a handy weapon, type hold(Weapon). to hold it!"), !.

describe(greatsword) :-
        write("A long sword shining with silver, always showing its unparalleled power, type hold(Weapon). to hold it!"), !.

describe(soulsword) :-
        write("A blood-red sword, crafted with utmost dedication by the supreme swordsmith five hundred years ago. However, due to the slaughter of too many beings, the blade has turned blood-red. , type hold(Weapon). to hold it!"), !.

describe(gold_coin) :-
        write("An expensive gold coin that can be used for trading"), !.

describe(the_map) :-
        show_image("./map.png"), !.

describe(bloodied_letter):-
        show_image("./blooded_letter.jpg"), !.

describe(diary):-
        show_image("./diary.png"), !.

describe(the_supreme_key):-
        write("A key of pure gold, shining with a dazzling light"), !.

describe(soul_eater_strategy) :-
        enemy_element_first(soul_eater, E1),
        format("'Adventurer's rumor: ~w really likes to use the ~w element.~n", [soul_eater, E1]), !.

describe(queen_of_the_moonlit_strategy) :-
        enemy_element_first(queen_of_the_moonlit, E1),
        enemy_element_second(queen_of_the_moonlit, E2),
        format("Adventurer's rumor: ~w is a one-of-a-kind genius, and her moves are unpredictable. If you don't have exceptionally strong vision, don't attempt lightly. However, she seems to be more skilled with the ~w element and the ~w element.~n", [queen_of_the_moonlit, E1, E2]), !.






describe(born_room) :-
        write("Here is the born place. There is a door to the south."),
        items_in_room(born_room), nl.

describe(freshman_room) :-
        write("You have reached the Dark Forest, There is a door to the south."), nl,
    (   enemy_alive(zombie, ZombieState),
        enemy_alive(army, ArmyState),
        ZombieState = alive,
        ArmyState = alive, at(army, freshman_room) ->
        write("army is happily holding a man, his face is very handsome, you don't know this man, but from his clothes, it seems to be a zombie before"), nl
    ;
        enemy_alive(zombie, ZombieState),
        enemy_alive(army, ArmyState),
        ZombieState = dead,
        ArmyState = alive, at(army, freshman_room) ->
        write("army sits beside zombie's dead body. You can't see his face, but you can feel his grief"), nl
    ;
        enemy_alive(zombie, ZombieState),
        ZombieState = alive ->
        write('There is a small zombie, type fight(zombie). to fight her. '), nl
    ;  
        write("The place is non-enemy."), nl
    ),

        findall(Item, (at(Item, freshman_room), \+is_enemy(Item), \+package(Item)), Items),
    (   Items \= [] ->
        write("You see the following items here: "), write(Items), nl
    ;   
        write("There are no items in this place."), nl
    ).
    




describe(central_room) :-
        write("You have reached the Central Waterfall, doors in all four directions."), nl,

        findall(Item, (at(Item, central_room), \+is_enemy(Item), \+package(Item)), Items),
    (   Items \= [] ->
        write("You see the following items here: "), write(Items), nl
    ;   
        write("There are no items in this place."), nl
    ).



describe(shop_room) :-
        write("You have reached the Shop, There is a door to the south."), nl,
        (   enemy_alive(army, State),
            State = alive ->
            write('There are an army and a businessman type interact(name). to interflow '), nl
        ;   
            write("The place is non-enemy."), nl
        ),
            findall(Item, (at(Item, shop_room), \+is_enemy(Item), \+package(Item)), Items),
        (   Items \= [] ->
            write("You see the following items here: "), write(Items), nl
        ;   
            write("There are no items in this place."), nl
        ).
    



describe(small_boss_room) :-
        write("You have reached the Darkest Dungeon, There's a door to the west of you"), nl,
        (   enemy_alive(soul_eater, State),
            State = alive ->
            write('Boss alert!!! There is a Soul Eater, you can type fight(soul_eater) to fight it.'), nl

        ;   
            write("The place is non-enemy."), nl
        ),
            findall(Item, (at(Item, small_boss_room), \+is_enemy(Item), \+package(Item)), Items),
        (   Items \= [] ->
            write("You see the following items here: "), write(Items), nl
        ;   
            write("There are no items in this place."), nl
        ).
    




describe(final_boss_room) :-
        write("You have reached the Moonlight castle, There's a door to the west of you"), nl,
        (   enemy_alive(queen_of_the_moonlit, State),
            State = alive ->
            write("Boss alert!!! Queen of the Moonlit here!!!!!, you can type fight(queen_of_the_moonlit) to fight it, and there's a door to your north and south"), nl

        ;   
            write("The place is non-enemy."), nl
        ),
            findall(Item, (at(Item, final_boss_room), \+is_enemy(Item), \+package(Item)), Items),
        (   Items \= [] ->
            write("You see the following items here: "), write(Items), nl
        ;   
            write("There are no items in this place."), nl
        ).
    



describe(treasure_room) :-
        write("You have reached the Precious derostis, There's a treasure box, try to use 'open' to open it, and there's a door to your north"), nl.



describe(_) :-
        write('It is a strange place.'), nl.




instructions :-
        nl,
        write("Welcome to Elemental Battle! You are the chosen warrior, gifted with the Eye of the Gods, granting you the ability to foresee the elemental powers of all your enemies. Prepare yourself to harness this power and face formidable foes in a world where every battle is defined by the mastery of elements."), nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('show_world_rule.   -- to show the game basic rule.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('fight(Enemy).      -- to fight the enemy'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('check(Object).     -- to view details about an item'), nl,
        write("open(Object).      -- to open the treasure box"), nl,
        write("display_package.   -- to check the contents of your backpack"), nl,
        write("interact(Sb).      -- to communicate with them"), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.

start :-
        instructions, !,
        initialize_attribute, !,
        look, !.


show_world_rule :-
        show_image("./elements.png"), 
        write("Fire control grass; grass control water; water control fire"), !.



show_image(FilePath) :-
    new(Window, picture('Image Display')),
    send(Window, size, size(1000, 1000)),  
    send(Window, display, new(_Bitmap, bitmap(FilePath))),  
    send(Window, open). 


describe(someplace) :- write('You are someplace.'), nl.

