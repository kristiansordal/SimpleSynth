import numpy as np

# Generate a sample input signal
N = 1000
t = np.linspace(0, 1, N)
x = np.sin(2 * np.pi * 50 * t) + np.sin(2 * np.pi * 120 * t)

# Perform the RFFT
X = np.fft.rfft(x)
print(X)

# Compute the frequency bins
freqs = np.fft.rfftfreq(N, d=1.0/N)

# Plot the magnitude spectrum
import matplotlib.pyplot as plt
plt.plot(freqs, np.abs(X))
plt.xlabel('Frequency (Hz)')
plt.ylabel('Magnitude')
plt.show()
