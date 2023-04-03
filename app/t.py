import numpy as np
import matplotlib.pyplot as plt

# Define the signal parameters
length = 10  # Length of the wave in seconds
fs = 1000  # Sampling rate in Hz
N = length * fs  # Number of samples
x = np.linspace(0, length, N, endpoint=False)  # Generate the time axis
y = np.sin(50 * 2 * np.pi * x) + 0.5 * np.sin(80 * 2 * np.pi * x)  # Generate the signal

# Calculate the FFT
Y = np.fft.fft(y)
freqs = np.fft.fftfreq(N, 1/fs)  # Generate the frequency axis
print(list(freqs))
# print(list(freqs[:N//2]))

# Plot the magnitude spectrum
fig, ax = plt.subplots()
ax.plot(freqs[:N//2], np.abs(Y[:N//2]) / N)  # Magnitude is normalized by the number of samples
ax.set_xlabel('Frequency (Hz)')
ax.set_ylabel('Magnitude')
plt.show()
