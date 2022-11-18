[y, fs]= audioread('knn.wav');
% plot(y)

% furior transformation
fy = fft(y);
aby = abs(fy);
i = (aby>50);
mult = fy. *i;
inv = ifft(mult);
ry = real(inv);
audiowrite('invf.wav',ry,fs)
audiowrite('fur.wav',fy,fs)
audiowrite('aby.wav',aby,fs)
audiowrite('inv.wav',inv,fs)
[c,fs]=audioread('invf.wav');
[a,fs]=audioread('fur.wav');
[b,fs]=audioread('aby.wav');
[d,fs]=audioread('inv.wav');
sound(a,fs)
sound(b,fs)
sound(c,fs)
sound(d,fs)