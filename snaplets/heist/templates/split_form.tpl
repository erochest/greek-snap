<apply template="base">
  <form method="GET" action="/split/download">
    <fieldset id="split1">
      <div class="page-header">
        <h1>Split <small>documents</small></h1>
      </div>
      <select multiple class="form-control" name="document" required size="20">
        <documentList>
          <option value="${documentId}"><documentTitle/></option>
        </documentList>
      </select>
      <div class="pull-right">
        <button id="splitnext1" class="btn btn-primary">
          Next <span class="glyphicon glyphicon-chevron-right"></span>
        </button>
      </div>
    </fieldset>

    <fieldset id="split2">
      <div class="page-header">
        <h1>Split <small>divisions</small></h1>
      </div>
      <ul class="list-group form-group">
        <li class="list-group-item">
          <label>
            <input type="radio" name="division" id="division1" value="document"/>
            <em>Documents</em> keep each dialogue as its own document.
          </label>
        </li>
        <li class="list-group-item">
          <label>
            <input type="radio" name="division" id="division2" value="section"/>
            <em>Sections</em> are smaller groupings within the document.
          </label>
        </li>
        <li class="list-group-item">
          <label>
            <input type="radio" name="division" id="division3" value="section"/>
            <em>Pages</em> are larger groupings within the document.
          </label>
        </li>
        <li class="list-group-item">
          <label>
            <input type="radio" name="division" id="division4" value="sp"/>
            <em>Speaking parts</em> are made by group everything each
            speaker's says in the dialogue into one document. Obviously, this
            does the most violence to the original.
          </label>
        </li>
      </ul>
      <div class="pull-right">
        <button id="splitnext2" class="btn btn-primary">
          Next <span class="glyphicon glyphicon-chevron-right"></span>
        </button>
      </div>
    </fieldset>

    <fieldset id="split3">
      <div class="page-header">
        <h1>Split <small>chunks</small></h1>
      </div>
      <div class="form-group">
        <label>
          <input type="text" name="chunksize" id="chunksize" value="250">
          <em>Chunk Size</em> is the number of tokens to include in each
          window of text.
        </label>
        <label>
          <input type="text" name="chunkoffset" id="chunkoffset" value="50">
          <em>Chunk Offset</em> is how far to slide the window over each
          step through the document.
        </label>
      </div>
      <div class="pull-right">
        <button id="splitdone" class="btn btn-success" type="submit">
          Done <span class="glyphicon glyphicon-ok"></span>
        </button>
      </div>
    </fieldset>
  </form>

  <div class="progress">
    <!-- Add .progress-bar-success just before submitting. -->
    <div class="progress-bar" role="progressbar" area-valuenow="0" area-valuemin="0" area-valuemax="3" style="width: 0%;">
      <span class="sr-only">0% Complete</span>
    </div>
  </div>

  <script src="/fay/split.js" language="javascript"></script>
</apply>
