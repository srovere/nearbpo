export ASTRA_DB_ID=48552c65-8c07-44b6-aa5c-74ef2fce002f
export ASTRA_DB_REGION=us-east1
export ASTRA_DB_KEYSPACE=public
export ASTRA_DB_APPLICATION_TOKEN=AstraCS:RawszStYieKGNvPbZbylktYX:d4250b373716022a09e8cf549d10e1c788df80d29bcead5ef1fe4a6a3d50f5c2

# Estaciones
#curl --request GET \
#    --url https://${ASTRA_DB_ID}-${ASTRA_DB_REGION}.apps.astra.datastax.com/api/rest/v1/keyspaces/${ASTRA_DB_KEYSPACE}/tables/stations/columns \
#    --header "x-cassandra-token: ${ASTRA_DB_APPLICATION_TOKEN}"
#curl --request GET \
#    --url https://${ASTRA_DB_ID}-${ASTRA_DB_REGION}.apps.astra.datastax.com/api/rest/v1/keyspaces/${ASTRA_DB_KEYSPACE}/tables/stations/rows \
#    --header "x-cassandra-token: ${ASTRA_DB_APPLICATION_TOKEN}"
#curl --request GET \
#    --url https://${ASTRA_DB_ID}-${ASTRA_DB_REGION}.apps.astra.datastax.com/api/rest/v1/keyspaces/${ASTRA_DB_KEYSPACE}/tables/stations/rows?pageState=BAABVXQA8H___5sA \
#    --header "x-cassandra-token: ${ASTRA_DB_APPLICATION_TOKEN}"

# Variables
#curl --request GET \
#    --url https://${ASTRA_DB_ID}-${ASTRA_DB_REGION}.apps.astra.datastax.com/api/rest/v1/keyspaces/${ASTRA_DB_KEYSPACE}/tables/variables/rows \
#    --header "x-cassandra-token: ${ASTRA_DB_APPLICATION_TOKEN}"

# Observations from station 87544
# --data '{ "pageSize": 1000, "filters": [ { "columnName": "station_id", "operator": "eq", "value": [ "87544" ] }, { "columnName": "status", "operator": "eq", "value": [ "A" ] } ] }'
#curl --request POST \
#    --url https://${ASTRA_DB_ID}-${ASTRA_DB_REGION}.apps.astra.datastax.com/api/rest/v1/keyspaces/${ASTRA_DB_KEYSPACE}/tables/observations/rows/query \
#    --header "Content-Type: application/json" \
#    --header "x-cassandra-token: ${ASTRA_DB_APPLICATION_TOKEN}" \
#    --data '{ "pageSize": 1000, "filters": [ { "columnName": "station_id", "operator": "eq", "value": [ "87544" ] } ] }'

# UPDATES
#curl --request PATCH \
#    --url https://${ASTRA_DB_ID}-${ASTRA_DB_REGION}.apps.astra.datastax.com/api/rest/v2/keyspaces/${ASTRA_DB_KEYSPACE}/observations/87544/tmax/2001-01-01 \
#    --header "Content-Type: application/json" \
#    --header "x-cassandra-token: ${ASTRA_DB_APPLICATION_TOKEN}" \
#    --data '{ "status": "A" }'
