# ☀️Solar Tape Reader
Read a collection of proprietary solar tape imagery from 8mm data tapes.

Extracts the headers and images.

## 📘Installation Instructions
1. Clone the repository.

```
git clone https://github.com/ChrisToumanian/solar_tape_reader
```

2. You will need Python >=3.8 and PIP. Check using `python -V`.

3. Create and enter a Python environment for the project.

```
cd solar_tape_reader
python -m venv venv
. venv/bin/activate
```

4. Install the required Python modules contained in requirements.txt.

```
pip install --upgrade pip
pip install -r requirements.txt
```

## How To Use
1. To ingest tapes navigate to `.\tape_controller` and run `.\extract_tape.ps1 <tape name> <start tape, usually 0> <number of tapes>` in PowerShell.
2. Move the DAT files created from `.\tape_controller\tape_data\` to `.\tape_data_staging`.
3. Open in WSL and run `./sunio/sunio -i -d -Ffits`.

## 📼Tape Deck Requirements
1. dd.exe to read from the tape deck - Windows dd.exe is available here: http://www.chrysocome.net/dd
2. mt.exe to rewind the tape - Windows mt.exe is available in a collection of utilities found here in LCUtils.zip: https://www.tecno-notas.com/winnt.htm
3. A tape deck - We're currently using a Sun XL 8mm Exebyte drive
4. SCSI PCI adapter - We're using an Adaptec AHA-2940UW with the drivers AdaptecAic78xx_for_AHA_29xx:
https://www.savagetaylor.com/2018/02/11/scsi-on-windows-10-adaptec-aha-2940-adaptec-29xx-ultra-or-aic-7870-adaptec-78xx/. It is an unsigned driver, so to install this on Windows 10 you must go to `Recovery Options` in the start menu and select `Advanced Startup - Restart now`. After the computer restarts select `System Restore` and press `7` to disable signed driver enforcement and press `Enter`. Once in Windows, go to the Device Manager, right-click on `Other devices - SCSI Controller` and select update driver and navigate to the driver directory.
