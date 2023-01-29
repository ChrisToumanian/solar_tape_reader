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
## 📼Tape Deck Requirements
1. dd.exe to read from the tape deck - Windows dd.exe is available here: http://www.chrysocome.net/dd
2. mt.exe to rewind the tape - Windows mt.exe is available in a collection of utilities found here in LCUtils.zip: https://www.tecno-notas.com/winnt.htm
3. A tape deck - We're currently using a Sun XL 8mm Exebyte drive
4. SCSI PCI adapter - We're using an Adaptec AHA-2940UW with the drivers AdaptecAic78xx_for_AHA_29xx:
https://www.savagetaylor.com/2018/02/11/scsi-on-windows-10-adaptec-aha-2940-adaptec-29xx-ultra-or-aic-7870-adaptec-78xx/
