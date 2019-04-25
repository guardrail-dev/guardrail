target="modules/sample/src/main/resources/issues/issue45.yaml"
min=23
max=23
cat >"${target}" <<!
swagger: "2.0"
info:
  title: Whatever
  version: 1.0.0
host: localhost:1234
schemes:
  - http
definitions:
  User:
    type: object
    required:
      - id
    properties:
      id:
        type: string
paths:
!

for c in $(seq "$min" "$max"); do
  echo -n '  /' >> "${target}"

# Can't go higher than 7 due to 22 parameter limit
  for i in $(seq 1 "$c"); do
    echo -n "{p${i}}/" >> "${target}"
  done

  echo : >> "${target}"

  cat >>"${target}" <<!
    get:
      operationId: ohNo_${c}
      x-scala-package: pathological
      produces:
        - application/json
      parameters:
!

  for i in $(seq 1 "$c"); do
    cat >>"${target}" <<!
      - name: p${i}
        in: path
        required: true
        type: string
      - name: q${i}
        in: query
        required: true
        type: string
      - name: h${i}
        in: header
        required: true
        type: string
!
  done

  cat >>"${target}" <<!
      responses:
        '200':
          schema:
            \$ref: '#/definitions/User'
!
done
