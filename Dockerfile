FROM haskell

COPY . .

RUN stack setup
RUN stack build --test

ENTRYPOINT ["stack"]
CMD ["run", "hray-exe"]
