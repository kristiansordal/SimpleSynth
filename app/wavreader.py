import scipy.io as si
import sys

def main():

    samplerate, data = si.wavfile.read(f'wavfiles/{sys.argv[1]}.wav') 
    sWave = list(map(sum, data))
    print(samplerate)
    print(sWave)
if __name__ == '__main__':
    main()
