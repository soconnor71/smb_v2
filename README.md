# About

Super Mario Bros. practice ROM for advanced players.

# Update v3.2 - 2018-10-20

Build server now running on i7-laptop. Should survive power-outage and is a million times faster.

** Practice Mode **
- **Fixed bug** where you would not always **restart the level** on the **same rule** as you entered.

** Scenarios **
- **Fixed bug** where scenarios could reach **"SUPER PLAYER"** even if you **missed input**.

** Web-UI **
- Only distribute as IPS.

# Update Web-UI - 2018-09-14

**Build-times are longer. Please allow for up to 20-second for the shitty raspberry to build your custom roms before clicking the link again. If you just want the default rom with no modifications, please click "Download Standard ROM" at the top of the page.**

** Practice Mode **
- Customizable Restart-level input
- Customizable Reset-ROM input
- **Allow 3-powerups** to be collected (Check the third on the **level you become small fire mario**).

** All Modes **
- Allow disable Music
- Allow disable SFX

# Update v3.1 - 2018-09-11

Thank you so much @darpey for all your help with this release :)

** Practice Mode **
- **Bug fix**. Small fire mario can now resume on a given rule from title screen.
- **Bug fix**. State not saved correctly when entering a new level.
- **Bug fix**. Rule-counter would only count to 999 then loop around.

# Update v3.0 - 2018-09-06

**FLAGPOLE** renamed to **SCENARIO**, and revamped.

** Practice Mode **
- Rework **powerup inputs**; while **paused**:
**Left** cycles between **Small, Super, and Fire Mario**.
**Down** flips the **size of mario** without changing actual powerup.
- Make it possible to select **Small Fire Mario** from the **Title screen** (3).
- If you **change powerup** while paused, it is **committed** to the **save state**.
- **Bug fix**. Beating a level would forget powerups.
- **Bug fix**. Resume on rule sometimes broken too (because of above bug).
- **Bug fix**. Loading would randomly mess up powerup state too.

** Scenario Mode **
Vastly improved the old FPG-practice engine. All scenarios are now described exactly at http://ud2.eu:5555/scenarios.html

- Reworked **FPG 1-1** to allow for **second stair pixel** too.
- Added **1-2g** practice scenario.
- Pressing **Start** returns to scenario select screen.
- Pressing **Select** restarts the current scenario.

# Update v2.2b - 2018-09-01
- Fix bug where sound quit working if loading while paused.
- Fix bug where starting as big on title screen didn't reflect correctly.
- Changed **size-toggle** while **paused** to **Down**.
- Added another face :)

# Update v2.2 - 2018-09-01
- Set **any powerup state** on the fly in **Practice**; while **paused**, press:
    - **Left**: Small mario, no power-up.
    - **Right**: Super Mario.
    - **Down**: Toggle size flag (make big or small, but keep powerup state).
    - **B**: Toggle fire-mode (ofc possible while small).
    - **A**: Give star-power.
- In **FPG**, replace frame-counter with **X Sub Speed**.
- Remove bug where loading while paused would softlock.
- Add more silly faces to loader :)

# Update v2.1 - 2018-08-08
- Fix bug where "resume from rule" put you on wrong rule.
- Hardcoded resume states for every 100 rules; **resuming** from rule 9099 now takes **~50 ms instead of 10 seconds** :)
- Playing as Luigi you are able to move again...
- Update rendering mode even if paused.

# Major Update v2.0 - 2018-07-31
- Change mapper to **MMC1** to allow modifications without having to remove bytes.
- Merge **Practice ROM** and **FPS ROM** into a **single ROM** (and included **Vanilla ROM** too).
## Practice ROM Updates
* Holding **Up** while pressing **Start** during gameplay will change to **sockfolder status**.
* Holding **A** while pressing **Start** at the menu will start you on **Second Quest**.
* Remove "Judge Radar" as it was retarded.

## FPS ROM Updates
* Actually **emulates** on a **specific setup** (rather than the arbitrary nonsense state that it was before). Only **1-1 D70** is currently **included**.
* Wrote a **LUA-script** to add own **custom configurations**. *Will describe in separate post.*
* Press **Left** and **Right** on title-screen to **select scenario** to practice.
* Press **Select** while dead to restart level.


# Update v1.7 - 2017-01-07

- **Fix bug** causing **state to become arbitrary** after entering a pipe (the Hammer Bro bug).

- Changed **restart level** to **Select + Up**

- Changed **restart game** to **Select + Down**

- Visually restore rule-counter @ restart level (Judge & Timer control are restored too, but not visually).

# Update v1.6 - 2017-12-30

## Judge-Radar

Upon beating a level, the rightmost column in the statusbar, "J", will show you what judge-frame you are on (for good/bad judges on 8-1). Only essential if you go to 8-1 from pipe.

## Sockfolder FPG compatible timer

Shows the timer control value in the same manner as Sockfolder FPG ROM under "x" in the statusbar. This is the value 0-K shown as level in Sockfolder FPG.

## Update frame & rule nr on jump

Previously frame and rule values were only updated when you land, now updated on jump too.

## Align statusbar with original SMB

Aligned "RM" and "TIME" to match "WORLD" and "TIME" in original SMB, so that visual queues are not destroyed.

# Update v1.5 - 2017-12-29

I hope you people had a jolly Christmas; also happy new year! <3

* No bugs! Resuming on a frame-rule in any mode should work, regardless of level! Please let me know if you find any resume-issues.
* Save-state re-enabled (completely untested). Select+Up to re-load level @ entered frame-rule.
* Select+Down to return to title-screen.
* Buildserver running on _ATROCIOUSLY slow_ rPI. Takes like 15 sec to build (and thus download) a tailored ROMs.

# Features
* **Detailed performance metrics**; frame rule, timer value, frame and frames left on frame-rule.
* Choose what **level and frame rule** to **start on**.
* **Predefined frame-rule** for each level.
* **Judge-radar** which shows what "Judge-frame" you entered the level at (for 8-1).
* **Quick return** to main-level for **level select**, no need to get up and reset console.
* Shows relative **x-position** (for wrong warp practice).
* Choose **nr of power-ups** collected.
* **Restart level** from Frame Rule you entered.
* Infinite lives.
* Play as **Luigi** :) Poor guy never gets to play.

# Controls
## Practice ROM -  Title screen
* Use **Select** to move up and down in the menu.
* Use **Left** and **Right** change world/level/p-ups.
* To change what frame rule to start from, use **Left** and **Right** to select position, and **Up** and **Down** to change it.
* Press **B** to set starting Rule to 0 (Will use current game rule if 0).
* Press **Start** as normal to start the game.
* Press **Up** when Rule is not selected to change to Luigi.
* Hold **A** while pressing **Start** to start on **Second Quest**.
- Set **any powerup state** on the fly; while **paused**, press:
**Left** to cycle between **Small, Super, and Fire Mario**.
**Down** to flip the **size of Mario** without changing actual powerup.

## Practice ROM - In-game
* Press **Select + Up** to restart level at entered Frame Rule.
* Press **Select + Down** to return to Title Screen.
* Holding **Up** while pressing **Start** to toggle rendering mode.

## FPS ROM - Title Screen
* Use **Left** and **Right** to change what setting to practice.
* **Start** to begin.

## FPS ROM - In-game
* Use **Select** while dead to restart the scenario.

# Display
From left to right, the top status bar displays are,

* Current frame-rule.
* Rule-timer at level entry (same as world letter in Sockfolder FPG).
* Current frame nr.
* Number of frames left on frame-rule when beating area.
* Relative X position.
* Standard game timer.

# Get it!

## From the build-server!

Build server allows you to easily create a custom version of the Practice ROM with pre-defined frame-rules for each level. Just paste your WSplit file and download your custom-built ROM image.

Build-server at: http://ud2.eu:5555

*To convert to Wsplit from other formats, you can use https://splits.io*

## Building from scratch

1. Clone pellsson/smb (github)
2. Clone pellsson/badassm into the smb directory (githib)
3. Make sure python3 is installed an accessible in path as "python".
4. Run make


**TY WOP0 WOP0** <3