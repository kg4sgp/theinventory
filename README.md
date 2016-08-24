# theinventory

This is a collection of components used for a simplistic inventory tracking
suite.

It currently consists of the following:

<table>
<tr>
  <th>Component</th>
  <th>Description</th>
  <th>Tooling</th>
  <th>License</th>
</tr>
<tr>
  <td><a href="./server/">server</a></td>
  <td>
    The API that stores inventory data in a database and allows access via
    HTTP + JSON.
  </td>
  <td>Haskell (servant)</td>
  <td>3-BSD</td>
</tr>
<tr>
  <td><a href="./android/">android</a></td>
  <td>
    The Android application, which also bundles a Scala/httpz library for
    accessing the API. (TODO: Unbundle this)
  </td>
  <td>Scala (sbt-android, httpz), Android tooling</td>
  <td>3-BSD</td>
</tr>
</table>
