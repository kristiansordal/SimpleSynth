import wave
import scipy.io as si
import matplotlib.pyplot as plt

def main():

    w = si.wavfile.read('wavfiles/SinePy.wav') 
    print(w[0])
    print(w[1])
    for x in w[1]:
        print(x)
    
    sWave = list(map(sum, w[1]))
    plt.plot(sWave)
    plt.show()
if __name__ == '__main__':
    main()
