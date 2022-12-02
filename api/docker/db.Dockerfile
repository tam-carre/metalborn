FROM postgres:15
COPY dbSchema.sql /docker-entrypoint-initdb.d/

